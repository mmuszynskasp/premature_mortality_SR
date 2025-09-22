############################################################################################################################
########################## example code for group of articles labelled in Rayyan as "YLL"
############################################################################################################################

rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(readr)
library(stringr)
library(purrr)
library(zoo)
library(tibble)
library(revtools)
library(reticulate)
library(bibliometrix)
library(stringr)

use_condaenv("bertopic_env", required = TRUE)
#use_condaenv("riscluster", required = TRUE)


data.dir <- "your directory"   ##### directory for review data
keywords.dir <- "your directory" ### directory wuth saved keywords
selected.dir <-  "your directory" ### dierectory for selected representative and random articles

setwd(data.dir)
ris_data <- read_bibliography("articles.ris") # read data saved from Rayyan

selection <- ris_data %>%
  mutate(n1_clean = str_remove(n1, fixed('RAYYAN-INCLUSION: {"Magdalena"=>"Included"} | RAYYAN-LABELS: '))) ## only articles labelled as "Included"


selection_yll <- selection %>%
  filter(str_detect(n1, "\\bYLL\\b"))

df <- selection_yll
df$text <- paste(df$title, df$abstract)
py$docs <- df$text



py_run_string("
from bertopic import BERTopic
from sentence_transformers import SentenceTransformer
from sklearn.feature_extraction.text import CountVectorizer
import pandas as pd

# Load pre-trained transformer
model = SentenceTransformer('all-MiniLM-L6-v2')

# Compute embeddings
embeddings = model.encode(docs, show_progress_bar=True)

# Fit BERTopic model
topic_model = BERTopic(verbose=True)
topics, probs = topic_model.fit_transform(docs, embeddings)

# Store results
topic_info = topic_model.get_topic_info()
document_info = pd.DataFrame({'text': docs, 'topic': topics})
")

# Get document-topic mapping
str(topics_df)
topic_assignments <- py$document_info
topics_df <- as.data.frame(topic_assignments)

# Add to your original R dataframe
df$topic <- topics_df$topic



###############select first one sub-group, which containts only rubish
py_run_string("
import re
from collections import Counter
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords

# Set of English stopwords and exclusions
english_stopwords = set(stopwords.words('english'))
exclusions = set(['death', 'deaths', 'mortality', 'premature',  'study','health', 'disease', 'among', 'analysis', 'life', 'based', 'years',
'associated', 'related', 'care', 'year', 'long', 'data', 'quality',
'specific', 'effects', 'women', 'outcomes', 'cost', 'impacts',
'alcohol', 'high', 'potential', 'results', 'time', 'evidence', 'first'])

# ISO country list
import pycountry
country_names = set([c.name.lower() for c in pycountry.countries])

def get_top_common_words(titles, min_length=4, top_n=10):
    words = []
    for title in titles:
        if title is None:
            continue
        tokens = re.findall(r'\\b[a-zA-Z]{4,}\\b', title.lower())
        for token in tokens:
            if (token not in english_stopwords and
                token not in exclusions and
                token not in country_names):
                words.append(token)
    counter = Counter(words)
    return counter.most_common(top_n)
")


# Convert Python result to data frame
top_words_df <- as.data.frame(do.call(rbind, py$top_words), stringsAsFactors = FALSE)
colnames(top_words_df) <- c("word", "frequency")

# Convert frequency column to numeric
top_words_df$frequency <- as.numeric(top_words_df$frequency)

words <- unlist(py$top_words)[seq(1, length(unlist(py$top_words)), 2)]

py_run_string("
import re
import string
from sklearn.feature_extraction.text import TfidfVectorizer, ENGLISH_STOP_WORDS
from sklearn.cluster import MiniBatchKMeans
import pycountry

# Excluded terms
excluded_terms = {
    'death', 'deaths', 'mortality', 'premature', 'study','health', 'disease', 'among', 'analysis',
    'life', 'based', 'years', 'associated', 'related', 'care', 'year', 'long', 'data', 'quality',
    'specific', 'effects', 'women', 'outcomes', 'cost', 'impacts', 'alcohol', 'high', 'potential',
    'results', 'time', 'evidence', 'first', 'low', 'yll', 'age'}

def normalize_and_filter(term):
    term = term.lower()
    term = term.translate(str.maketrans('', '', string.punctuation))
    return [w for w in term.split() if re.fullmatch(r'[a-zA-Z]{3,}', w)]

# Country name parts
country_words = set()
for country in pycountry.countries:
    country_words.update(normalize_and_filter(country.name))

custom_stopwords = ENGLISH_STOP_WORDS.union(country_words).union(excluded_terms)
custom_stopwords = list(custom_stopwords)

# TF-IDF
vectorizer = TfidfVectorizer(
    stop_words=custom_stopwords,
    max_df=0.8,
    min_df=5,
    max_features=10000,
    token_pattern=r'\\b[a-zA-Z]{3,}\\b'
)
X = vectorizer.fit_transform(titles)

# Clustering
n_clusters = 10
kmeans = MiniBatchKMeans(n_clusters=n_clusters, random_state=42, batch_size=2048)
kmeans.fit(X)

feature_names = vectorizer.get_feature_names_out()
order_centroids = kmeans.cluster_centers_.argsort()[:, ::-1]

# Top keywords per cluster
top_keywords = []
for i in range(n_clusters):
    raw_keywords = [feature_names[ind] for ind in order_centroids[i, :12]]
    top_keywords.append(raw_keywords[:6])

labels = kmeans.labels_
# ✅ Export to R
r.labels = kmeans.labels_
r.top_keywords = top_keywords
")


cluster_ids_t <- py$labels
top_keywords_t <- py$top_keywords


selection_yll$cluster <- cluster_ids_t

sampled_selection <- selection_yll %>%
  group_by(cluster) %>%
  slice_sample(n = 4) %>%
  ungroup()

############################################################################
#for observations with no abstract, add to the group with least articles 
# Determine which cluster has the most articles
cluster_table <- table(cluster_ids)
smallest <- as.integer(names(cluster_table)[which.min(cluster_table)])
# Initialize the cluster column
selection_yll$cluster <- NA
# Assign clusters to rows with abstracts
selection_yll$cluster[!is.na(selection_yll$abstract)] <- cluster_ids
# Assign the largest cluster to missing abstracts
selection_yll$cluster[is.na(selection_yll$abstract)] <- smallest

##############################################################################
##write out
setwd(data.dir)
write_bibliography(ris_data, file = "YLL.ris")

#######
cluster_keywords <- do.call(rbind, lapply(top_keywords, function(x) {
  length(x) <- 6 
  return(x)
}))

cluster_keywords <- as.data.frame(cluster_keywords, stringsAsFactors = FALSE)
colnames(cluster_keywords) <- paste0("keyword_", 1:6)
cluster_keywords$cluster <- 0:(nrow(cluster_keywords) - 1)  
cluster_keywords <- cluster_keywords[, c("cluster", paste0("keyword_", 1:6))]

# Save to CSV
setwd(keywords.dir)
write.csv(cluster_keywords, "YLL.csv", row.names = FALSE)

#################################################################################
################## random selection

set.seed(42)  # for reproducibility
# Attach cluster IDs to ris_data (preserving missing abstracts)
cluster_ids_full <- rep(NA, nrow(ris_data))
non_missing <- !is.na(ris_data$abstract)
cluster_ids_full[non_missing] <- py$labels
ris_data$cluster_id <- cluster_ids_full

# Count articles per cluster (exclude NAs)
cluster_sizes <- table(na.omit(cluster_ids_full))
top3_clusters <- as.integer(names(sort(cluster_sizes, decreasing = TRUE)[1:3]))

# Initialize vector for sampled indices
sampled_indices <- c()

# Sample 4 from each cluster (or fewer if not enough)
for (k in 0:9) {
  in_cluster <- which(ris_data$cluster_id == k)
  n_sample <- min(4, length(in_cluster))
  sampled_indices <- c(sampled_indices, sample(in_cluster, n_sample))
}

# Sample 1 extra from each of the top 3 clusters (not already sampled)
for (k in top3_clusters) {
  available <- setdiff(which(ris_data$cluster_id == k), sampled_indices)
  if (length(available) >= 1) {
    sampled_indices <- c(sampled_indices, sample(available, 1))
  }
}

# Extract sampled articles
sampled_articles <- ris_data[sampled_indices, ]

####add 4 because of missing texts available, language etc.
# Sample 4 new articles not in sampled_articles
remaining <- ris_data[!(ris_data$doi %in% sampled_articles$doi), ]
extra_sample <- remaining[sample(nrow(remaining), 4), ]

# Combine into an updated sample
sampled_articles <- rbind(sampled_articles[,1:23], extra_sample[,-24])

setwd(selected.dir)
write_bibliography(sampled_articles, file = "sampleYLL.ris")



