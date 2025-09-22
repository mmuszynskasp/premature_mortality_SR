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

# load the Python script
use_condaenv("bertopic_env", required = TRUE)
source_python("run_bertopic.py")  


data.dir <- "your directory"   ##### directory for review data
##########################################################
#prepare data
setwd(data.dir)
ris_data <- read_bibliography("articles.ris")

selection <- ris_data %>%
  mutate(n1_clean = str_remove(n1, fixed('RAYYAN-INCLUSION: {"Magdalena"=>"Included"} | RAYYAN-LABELS: ')))



selection_no <- selection %>%
  filter(!(str_detect(n1, "\\bgbd\\b") |str_detect(n1, "\\bYYL\\b") |str_detect(n1, "\\bhazard\\b") 
           |str_detect(n1, "\\brisk\\b")|str_detect(n1, "\\bgroup\\b")|str_detect(n1, "\\bexcess\\b")|str_detect(n1, "\\bavoidable\\b")|str_detect(n1, "\\bages\\b")|str_detect(n1, "\\byes\\b")))


df <- selection_no
df$text <- paste(df$title, df$abstract)
py$docs <- df$text

#############################################################
####### run the Python script
####### it is done in two steps: 1.create groups of similar article, 2.for the group of rest articles "-1" create groups again
###### for each group we select 1. representative article chosen by the program in python; 2. one randomly sampled article


######## step 1
result <- run_bertopic(py$docs) # call the function
# extract the results
topic_info <- result[[1]]
document_info <- result[[2]]

# extract representative texts
rep_text_main <- result[[3]]  # main representative_docs
example_texts_main <- sapply(rep_text_main, function(x) if (length(x) > 0) x[[1]] else NA)
example_texts_main <- tolower(example_texts_main)

# Match to original data
match_indices_main <- match(example_texts_main, df$combined_text)

# Add columns to topic_info_main
topic_info_main <- result[[1]]
topic_info_main$Representative_Docs <- rep_text_main
topic_info_main$title <- df$title[match_indices_main]
topic_info_main$abstract <- df$abstract[match_indices_main]



####### step 2
#prepare the data
generic_docs <- df$text[document_info$topic == -1]
generic_docs <- tolower(generic_docs)
py$docs_sub <- generic_docs

result_sub <- run_bertopic(py$docs_sub)

topic_info_sub <- result_sub[[1]]
document_info_sub <- result_sub[[2]]

saveRDS(topic_info, "no_topic_info_no.rds")
saveRDS(document_info, "no_document_info_no.rds")
saveRDS(topic_info_sub, "no_topic_info_sub_no.rds")
saveRDS(document_info_sub, "no_document_info_sub_no.rds")



df$combined_text <- tolower(df$text) # preprocess original text in case


#### STEP 2: Handle SUBTOPIC topic_info ####

# Extract representative texts
rep_text_sub <- result_sub[[3]]  # subtopic representative_docs
example_texts_sub <- sapply(rep_text_sub, function(x) if (length(x) > 0) x[[1]] else NA)
example_texts_sub <- tolower(example_texts_sub)

# Match to original data
match_indices_sub <- match(example_texts_sub, df$combined_text)

# Add columns to topic_info_sub
topic_info_sub <- result_sub[[1]]
topic_info_sub$Representative_Docs <- rep_text_sub
topic_info_sub$title <- df$title[match_indices_sub]
topic_info_sub$abstract <- df$abstract[match_indices_sub]

#### STEP 3: Combine and clean ####
# Remove topic -1 from main topics
topic_info_main_clean <- subset(topic_info_main, Topic != -1)
# Combine both sets
topic_info_final <- rbind(topic_info_main_clean, topic_info_sub)

#topic_info_final <- topic_info_main


##############################################################################################
# export back to Rayyan ----representative articles
library(RefManageR)

# Step 1: Match topic_info_final titles to your full selection data
bib_df <- topic_info_final %>%
  select(title) %>%
  inner_join(selection, by = "title") %>%
  distinct(title, .keep_all = TRUE)

# Step 2: Prepare fields
bib_df <- bib_df %>%
  mutate(
    bibtype = "Article",
    bibkey = make.names(title),
    author = ifelse(!is.null(author), sapply(author, function(x) paste(x, collapse = " and ")), "Unknown"),
    journal = ifelse("journal" %in% colnames(.), as.character(journal), "Unknown"),
    year = ifelse("year" %in% colnames(.), as.character(year), "n.d."),
    title = as.character(title),
    doi = if ("doi" %in% colnames(.)) as.character(doi) else "",
    abstract = if ("abstract" %in% colnames(.)) as.character(abstract) else ""
  )

# Step 3: Create one BibEntry per row
bib_entries <- do.call("c", lapply(seq_len(nrow(bib_df)), function(i) {
  BibEntry(
    bibtype = bib_df$bibtype[i],
    key = bib_df$bibkey[i],
    title = bib_df$title[i],
    author = bib_df$author[i],
    year = bib_df$year[i],
    journal = bib_df$journal[i],
    doi = bib_df$doi[i],
    abstract = bib_df$abstract[i]
  )
}))

# Step 4: Save to BibTeX file
WriteBib(bib_entries, file = "rayyan_ready_no.bib")

##############################################randomly pick one article per no
sampled_set <- document_info %>%
  filter(topic!=-1) %>%
  group_by(topic) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  add_row(document_info_sub %>%
            group_by(topic) %>%
            slice_sample(n = 1) %>%
            ungroup())

# 1. Reconstruct combined text for matching
selection <- selection %>%
  mutate(combined_text = paste(title, abstract) %>% tolower())

# 2. Join sampled_set with full metadata
sampled_set_joined <- sampled_set %>%
  mutate(text = tolower(text)) %>%
  left_join(selection, by = c("text" = "combined_text"))

# Step 1: Clean and validate fields
sampled_bib <- sampled_set_joined %>%
  mutate(
    bibtype = "Article",
    bibkey = make.names(title),
    author = if ("author" %in% names(.)) {
      sapply(author, function(a) {
        if (is.null(a) || all(is.na(a))) {
          "Unknown"
        } else if (is.character(a)) {
          paste(a, collapse = " and ")
        } else if (is.list(a)) {
          paste(unlist(a), collapse = " and ")
        } else {
          "Unknown"
        }
      })
    } else {
      "Unknown"
    },
    journal = if ("journal" %in% names(.)) as.character(journal) else NA_character_,
    year = if ("year" %in% names(.)) as.character(year) else NA_character_,
    title = as.character(title),
    doi = if ("doi" %in% names(.)) as.character(doi) else "",
    abstract = if ("abstract" %in% names(.)) as.character(abstract) else ""
  )

# Step 2: Filter rows with valid required fields
valid_bib <- sampled_bib %>%
  filter(!is.na(journal), journal != "", !is.na(year), year != "")

# Step 3: Log dropped rows (optional)
dropped <- anti_join(sampled_bib, valid_bib, by = "title")
cat("Dropped rows due to missing journal or year:\n")
print(dropped$title)

# Step 4: Create BibEntry list
bib_entries <- do.call("c", lapply(seq_len(nrow(valid_bib)), function(i) {
  BibEntry(
    bibtype = valid_bib$bibtype[i],
    key     = valid_bib$bibkey[i],
    title   = valid_bib$title[i],
    author  = valid_bib$author[i],
    year    = valid_bib$year[i],
    journal = valid_bib$journal[i],
    doi     = valid_bib$doi[i],
    abstract= valid_bib$abstract[i]
  )
}))

# Step 5: Save BibTeX file
WriteBib(bib_entries, file = "sampled_articles_no.bib")

