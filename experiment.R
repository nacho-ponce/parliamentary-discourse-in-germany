options(stringAsFactors = FALSE)

library(quanteda)
library(topicmodels)

# Load speeches dataset
textdata <- read.csv("data/legislatures/speeches_leg_x.csv", sep = ",", encoding = "UTF-8")

# Build corpus
sotu_corpus <- corpus(textdata$speechContent, docnames = textdata$id)

# Load dictionary of lemmas for German
lemma_data <- read.table("data/resources/lemmatization-de.txt", header = TRUE, sep = "\t", row.names=NULL)

# Load German stopword list
stopwords_extended <- readLines("data/resources/stop_words_german.txt", encoding = "UTF-8")

# Create DTM
corpus_tokens <- sotu_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

# Detect frequent collocations
sotu_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 25)
sotu_collocations <- sotu_collocations[1:250, ]

corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)

# Build DTM
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

dim(DTM)

# Remove common words
top_terms <- c("herr", "frau", "kollege", "neu",
"deutchland", "dr", "geben", "kind")
DTM <- DTM[, !(colnames(DTM) %in% top_terms)]

# Filter out empty documents
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

require(topicmodels)

# Train LDA
K <- 10

topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02
))

tmResult <- posterior(topicModel)

attributes(tmResult)

ncol(DTM)

beta <- tmResult$terms
dim(beta)
rowSums(beta)

nrow(DTM)
theta <- tmResult$topics
dim(theta)
rowSums(theta)[1:10]

# Save topic terms
sink(file = "results/legx.txt")
terms(topicModel, 10)
sink()

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

# Visualization
library(LDAvis)
library("tsne")

svd_tsne <- function(x) tsne(svd(x)$u)

json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
  vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
  plot.opts = list(xlab = "", ylab = ""))

serVis(json)
