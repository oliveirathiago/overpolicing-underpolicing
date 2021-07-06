######
# Supervised classification of open-ended responses
# Thiago R. Oliveira
#####

## Supervised models

## Load packages
library(foreign)        # read .sav dataset
library(readxl)         # read excel files
library(dplyr)          # data manipulation
library(ngram)          # count words
library(quanteda)       # text analysis | cleaning text data
library(lda)            # latent dirichlet allocation
library(ggplot2)        # ggplot
library(caret)          # machine learning
library(tidyr)

########

# Load dataset
painel <- read.spss('data/painelaber_1.sav',
                    to.data.frame = T)[, -6] %>%
  mutate(id_match = paste(quest, onda, sep = "."))
  

# Load training data
training_law <- read_xlsx('data/training_law.xlsx')[, c(4, 14)]
load('data/training_pol.RData')

# Rename key variables
painel <- painel %>%
  rename('dutylaw' = 'P24',
         'dutylaw_text' = 'P24A',
         'dutypol' = 'P34',
         'dutypol_text' = 'P35')

# Rename variables - training data
training_law <- training_law %>%
  rename('dutylaw_text' = 'P24A',
         'topics_law' = 'Decisão final do comitê central')
training_law$topics_law <- na_if(training_law$topics_law, '??')
training_law$topics_law <- na_if(training_law$topics_law, 'Missing')
training_law$topics_law <- na_if(training_law$topics_law, 'Não devem ser obedecidas')
training_law$topics_law <- as.factor(training_law$topics_law)
training_pol <- training_pol %>%
  rename('dutypol_text' = 'P35',
         'topics_pol' = 'obey_quali_4cats')
training_pol$topics_pol <- as.factor(training_pol$topics_pol)


# Create new ID variables
painel <- mutate(painel, id = rownames(painel))

# Replace DK/DA with NAs
#painel <- painel %>%
#  mutate_at(vars(dutylaw, dutypol), na_if, 8) %>%
#  mutate_at(vars(dutylaw, dutypol), na_if, 9)

for (i in c('dutylaw', 'dutypol')) {
  painel[,i] <- na_if(painel[,i], c('Não sabe'))
  painel[,i] <- na_if(painel[,i], c('Não respondeu'))
}

# Make yes/no indicators logical
painel$dutylaw <- ifelse(painel$dutylaw == 'Devem ser obedecidas', T, F)
painel$dutypol <- ifelse(painel$dutypol == 'Sim, deve obedecer', T, F)

# Clean text data
for (i in c('dutylaw_text', 'dutypol_text')) {
  painel[, i] <- as.character(painel[,i])
  painel[, i] <- enc2utf8(painel[, i])
  painel[, i] <- tolower(painel[, i])
  for (j in c(1:nrow(painel))) {
    if (wordcount(painel[,i][j]) == 0) {
      painel[,i][j] <- NA
    }
  }
}

# Clean text data: training data
## Law:
training_law$dutylaw_text <- as.character(training_law$dutylaw_text)
training_law$dutylaw_text <- enc2utf8(training_law$dutylaw_text)
training_law$dutylaw_text <- tolower(training_law$dutylaw_text)
for (j in c(1:1806)) {
  if (wordcount(training_law$dutylaw_text[j]) == 0) {
    training_law$dutylaw_text[j] <- NA 
  }
}
## Police:
training_pol$dutypol_text <- as.character(training_pol$dutypol_text)
training_pol$dutypol_text <- enc2utf8(training_pol$dutypol_text)
training_pol$dutypol_text <- tolower(training_pol$dutypol_text)
for (j in c(1:1806)) {
  if (wordcount(training_pol$dutypol_text[j]) == 0) {
    training_pol$dutypol_text[j] <- NA 
  }
}

# data.frame removing empty open-ended responses
painel_predfm_law <- painel[!is.na(painel$dutylaw_text),]
painel_predfm_pol <- painel[!is.na(painel$dutypol_text),]

training_law <- training_law[!is.na(training_law$dutylaw_text),]
training_pol <- training_pol[!is.na(training_pol$dutypol_text),]

# Filtering 'no' answers
painel_predfm_pol_no <- painel_predfm_pol %>% 
  filter(dutypol == F)

# Filtering only 'yes' responses
painel_predfm_law <- painel_predfm_law %>% 
  filter(dutylaw == T)
painel_predfm_pol <- painel_predfm_pol %>% 
  filter(dutypol == T)

# Filtering training data (no's)
training_pol_no <- training_pol %>%
  filter(topics_pol == 2 |
           topics_pol == 4)

# Filtering training data
training_law <- training_law %>%
  filter(topics_law == 1 |
           topics_law == 2)
training_pol <- training_pol %>%
  filter(topics_pol == 1 |
           topics_pol == 3)

# Filtering only relevant columns
painel_predfm_law <- painel_predfm_law[, c('dutylaw_text', 'id')]
painel_predfm_pol <- painel_predfm_pol[, c('dutypol_text', 'id')]
painel_predfm_pol_no <- painel_predfm_pol_no[, c('dutypol_text', 'id')]

# Create id variable
painel_predfm_law <- mutate(painel_predfm_law, id_law = rownames(painel_predfm_law))
painel_predfm_pol <- mutate(painel_predfm_pol, id_pol = rownames(painel_predfm_pol))
painel_predfm_pol_no <- mutate(painel_predfm_pol_no, id_pol = rownames(painel_predfm_pol_no))
training_law <- mutate(training_law, id = rownames(training_law))
training_pol <- mutate(training_pol, id = rownames(training_pol))
training_pol_no <- mutate(training_pol_no, id = rownames(training_pol_no))

# Create DFM: training data
law_train <- dfm(training_law$dutylaw_text, remove_punct = TRUE, stem = T, remove = stopwords('portuguese'))
pol_train <- dfm(training_pol$dutypol_text, remove_punct = TRUE, stem = T, remove = stopwords('portuguese'))
pol_train_no <- dfm(training_pol_no$dutypol_text, remove_punct = TRUE, stem = T, remove = stopwords('portuguese'))

# Create DFM: test data
law_text <- dfm(painel_predfm_law$dutylaw_text, remove_punct = TRUE, stem = T, select = featnames(pol_train))
pol_text <- dfm(painel_predfm_pol$dutypol_text, remove_punct = TRUE, stem = T, select = featnames(pol_train))
pol_text_no <- dfm(painel_predfm_pol_no$dutypol_text, remove_punct = TRUE, stem = T, select = featnames(pol_train_no))

# Create final DFMs with equal ncols
law <- dfm(c(training_law$dutylaw_text, painel_predfm_law$dutylaw_text), remove_punct = TRUE, stem = T, remove = stopwords('portuguese'))
pol <- dfm(c(training_pol$dutypol_text, painel_predfm_pol$dutypol_text), remove_punct = TRUE, stem = T, remove = stopwords('portuguese'))
pol_no <- dfm(c(training_pol_no$dutypol_text, painel_predfm_pol_no$dutypol_text), remove_punct = TRUE, stem = T, remove = stopwords('portuguese'))

docvars(pol, "set") <- c(rep("train", nrow(training_pol)), rep("new", nrow(painel_predfm_pol)))
docvars(law, "set") <- c(rep("train", nrow(training_law)), rep("new", nrow(painel_predfm_law)))
docvars(pol_no, "set") <- c(rep("train", nrow(training_pol_no)), rep("new", nrow(painel_predfm_pol_no)))

# Testing new DFMs:
dfm_subset(law, docvars(law, "set") == "train") %>% featfreq()
all.equal(featnames(law_train), featnames(law_text), )

dfm_subset(pol, docvars(pol, "set") == "train") %>% featfreq()
all.equal(featnames(pol_train), featnames(pol_text), )

dfm_subset(pol_no, docvars(pol_no, "set") == "train") %>% featfreq()
all.equal(featnames(pol_train_no), featnames(pol_text_no), )

#########################

# Training model: law

law_mtrain <- train(x = as.matrix(dfm_subset(law, docvars(law, "set") == "train")),
                    y = factor(training_law$topics_law),
                    method = "svmLinearWeights2") 
law_mtrain

# Predicting model: law
law_predicted <- predict(object = law_mtrain, newdata = as.matrix(dfm_subset(law, docvars(law, "set") == "new")))

# Assign predicted groups to dataset
painel_postdfm_law <- painel_predfm_law
painel_postdfm_law$topic_law <- law_predicted

# Training model: police

pol_mtrain <- train(x = as.matrix(dfm_subset(pol, docvars(pol, "set") == "train")),
                    y = factor(training_pol$topics_pol),
                    method = "svmLinearWeights2") 
pol_mtrain

# Predicting model: police
pol_predicted <- predict(object = pol_mtrain, newdata = as.matrix(dfm_subset(pol, docvars(pol, "set") == "new")))

# Assign predicted groups to dataset
painel_postdfm_pol <-painel_predfm_pol
painel_postdfm_pol$topic_police <- pol_predicted



# Training model: police NO's

pol_mtrain_no <- train(x = as.matrix(dfm_subset(pol_no, docvars(pol_no, "set") == "train")),
                    y = factor(training_pol_no$topics_pol),
                    method = "svmLinearWeights2") 
pol_mtrain_no

# Predicting model: police NO's
pol_predicted_no <- predict(object = pol_mtrain_no, newdata = as.matrix(dfm_subset(pol_no, docvars(pol_no, "set") == "new")))

# Assign predicted groups to dataset NO's
painel_postdfm_pol_no <-painel_predfm_pol_no
painel_postdfm_pol_no$topic_policeNO <- pol_predicted_no


painel_with_topics <- left_join(painel, painel_postdfm_law[, c('id', 'topic_law')], by = 'id')
painel_with_topics <- left_join(painel_with_topics, painel_postdfm_pol[, c('id', 'topic_police')], by = 'id')
painel_with_topics <- left_join(painel_with_topics, painel_postdfm_pol_no[, c('id', 'topic_policeNO')], by = 'id')

painel_with_topics <- painel_with_topics %>%
  mutate(final_topics = case_when(
    topic_police == 1 ~ 'Consent',
    topic_police == 3 ~ 'Coercion',
    topic_policeNO == 2 ~ 'Protest',
    topic_policeNO == 4 ~ 'Rejection',
    TRUE ~ as.character(NA))
  )
  
  
# Assigning a third category to no's
painel_with_topics <- painel_with_topics %>%
  replace_na(list(topic_law = '5')) %>%
  mutate_at(vars(topic_law), list(~case_when(
    . == '1' ~ 'Consent',
    . == '2' ~ 'Coercion',
    . == '5' ~ 'No duty',
    TRUE ~ as.character(NA))))


save(painel_with_topics, file = 'data/painel_with_topics_jan.RData')
