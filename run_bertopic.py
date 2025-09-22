from bertopic import BERTopic
from sklearn.feature_extraction.text import CountVectorizer
from sentence_transformers import SentenceTransformer
import nltk
from nltk.corpus import stopwords
import pandas as pd

def run_bertopic(docs):
    # Download stopwords
    nltk.download('stopwords')
    stop_words = stopwords.words('english')

    # Country names
    country_names = [
        'afghanistan', 'albania', 'algeria', 'andorra', 'angola', 'antigua', 'argentina', 'armenia', 'australia', 'austria',
        'azerbaijan', 'bahamas', 'bahrain', 'bangladesh', 'barbados', 'belarus', 'belgium', 'belize', 'benin', 'bhutan',
        'bolivia', 'bosnia', 'botswana', 'brazil', 'brunei', 'bulgaria', 'burkina', 'burundi', 'cambodia', 'cameroon',
        'canada', 'chad', 'chile', 'china', 'colombia', 'comoros', 'congo', 'croatia', 'cuba', 'cyprus', 'czech',
        'denmark', 'djibouti', 'dominica', 'ecuador', 'egypt', 'england', 'eritrea', 'estonia', 'eswatini', 'ethiopia', 'fiji',
        'finland', 'france', 'gabon', 'gambia', 'georgia', 'germany', 'ghana', 'greece', 'grenada', 'guatemala', 'guinea',
        'guyana', 'haiti', 'honduras', 'hungary', 'iceland', 'india', 'indonesia', 'iran', 'iraq', 'ireland', 'israel',
        'italy', 'jamaica', 'japan', 'jordan', 'kazakhstan', 'kenya', 'kiribati', 'korea', 'kuwait', 'kyrgyzstan',
        'laos', 'latvia', 'lebanon', 'lesotho', 'liberia', 'libya', 'liechtenstein', 'lithuania', 'luxembourg',
        'madagascar', 'malawi', 'malaysia', 'maldives', 'mali', 'malta', 'mauritania', 'mauritius', 'mexico',
        'micronesia', 'moldova', 'monaco', 'mongolia', 'montenegro', 'morocco', 'mozambique', 'myanmar', 'namibia',
        'nauru', 'nepal', 'netherlands', 'new', 'zealand', 'nicaragua', 'niger', 'nigeria', 'north', 'macedonia',
        'norway', 'oman', 'pakistan', 'palau', 'panama', 'papua', 'paraguay', 'peru', 'philippines', 'poland',
        'portugal', 'qatar', 'romania', 'russia', 'rwanda', 'saint', 'kitts', 'lucia', 'vincent', 'samoa', 'san',
        'sao', 'saudi', 'senegal', 'serbia', 'seychelles', 'sierra', 'singapore', 'slovakia', 'slovenia', 'solomon',
        'somalia', 'south', 'africa', 'spain', 'sri', 'lanka', 'sudan', 'suriname', 'sweden', 'switzerland', 'syria',
        'taiwan', 'tajikistan', 'tanzania', 'thailand', 'timor', 'togo', 'tonga', 'trinidad', 'tunisia', 'turkey',
        'turkmenistan', 'tuvalu', 'uganda', 'ukraine', 'united', 'arab', 'emirates', 'kingdom', 'states', 'uruguay',
        'uzbekistan', 'vanuatu', 'venezuela', 'vietnam', 'yemen', 'zambia', 'zimbabwe', 'glasgow', 'scotland', 'italian', 'spanish', 
        'brazilian', 'paulo', 'janeiro', 'rio', 'kong','hong', 'istanbul','bulgarian','belgian','countries', 'russian','mexican','nih','funding'
    ]

    # Add years and domain-specific terms
    years = [str(y) for y in range(1950, 2035)]
    domain_terms = [
        'mortality', 'study', 'health', 'risk', 'death', 'deaths', 'diseases', 'premature', 'differences',
        'data', 'years', 'life', 'lost', 'yll', 'dot', 'disease', 'burden',
        'due', 'type', 'age', 'korean', 'thousands', 'center', 'research', "million",'global','per','disability','population','rates','rate','results',
        'delhi','beijing','among','factors'
        #'old', 'people','per'
        #'patients', 'relative',
        #'associated', 'cancer', 'among', 'premature', 'days', 'parents','results','cox','increased','allcause'
    #    'life', 'lost', 'yll',
    ]

    # Combine all stopwords
    custom_stopwords = stop_words + country_names + years + domain_terms

    # Preprocess docs
    docs = [doc.lower() for doc in docs]

    # Vectorizer
    vectorizer_model = CountVectorizer(
        stop_words=custom_stopwords,
        token_pattern=r"(?u)\b[a-z]{3,}\b"
    )

    # Sentence embeddings
    model = SentenceTransformer('all-MiniLM-L6-v2')
    embeddings = model.encode(docs, show_progress_bar=True)

    # BERTopic
    topic_model = BERTopic(vectorizer_model=vectorizer_model, verbose=True)
    topics, probs = topic_model.fit_transform(docs, embeddings)

    # Outputs
    topic_info = topic_model.get_topic_info()
    representative_docs = topic_model.get_representative_docs()
    document_info = pd.DataFrame({'text': docs, 'topic': topics})

    return topic_info, document_info, representative_docs
