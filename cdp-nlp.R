
### Libraries
# Basic
library(rio)
library(foreign)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(stringi)  
library(utf8)
# Network
library(igraph)
library(tnet)
# NLP
library(quanteda)
library(quanteda.textmodels)
library(glmnet)
library(caret)
library(translateR)
library(cleanNLP)
library(reticulate)
library(stopwords)
require(seededlda)
require(lubridate)
library(pdftools)
library(tm)
library(cld3)
library(stm)
# Other
library(arules)
library(arulesViz)
library(reshape2)
library(quantmod)

# install.packages("devtools")


# --- CITIES
# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Cities/Cities Responses'))

# --- Load the 2020 data
df_cities.20 <- read.csv('2020_Full_Cities_Dataset.csv') %>% 
  dplyr::select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
         'region' = CDP.Region, 'qstn' = Question.Number,
         'qstname' = Question.Name, 'coln' = Column.Number, 'colname' = Column.Name,
         'rown' = Row.Number, 'rowname' = Row.Name, 'resp' = Response.Answer)
# Recode to UTF-8
temp_vec <- df_cities.20$resp
Encoding(temp_vec) <- "UTF-8"
df_cities.20$resp <- temp_vec
temp_vec <- df_cities.20$colname
Encoding(temp_vec) <- "UTF-8"
df_cities.20$colname <- temp_vec
# Recode 'Other' category
df_cities.20$resp[grepl("^Other, please specify", as.character(df_cities.20$resp))] <- "Other"
# Calculate question with longest answer
df_cities.20$len <- unlist(lapply(df_cities.20$resp, nchar))
t <- df_cities.20 %>% group_by(qstname) %>% summarise(mean_len=mean(len, na.rm=T))

# --- Load the 2019 data
df_cities.19 <- read.csv('2019_Full_Cities_Dataset.csv') %>% 
  dplyr::select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
         'region' = CDP.Region, 'qstn' = Question.Number,
         'qstname' = Question.Name, 'coln' = Column.Number, 'colname' = Column.Name,
         'rown' = Row.Number, 'rowname' = Row.Name, 'resp' = Response.Answer)
# Recode to UTF-8
temp_vec <- df_cities.19$resp
Encoding(temp_vec) <- "UTF-8"
df_cities.19$resp <- temp_vec
temp_vec <- df_cities.19$colname
Encoding(temp_vec) <- "UTF-8"
df_cities.19$colname <- temp_vec
# Recode 'Other' category
df_cities.19$resp[grepl("^Other:", as.character(df_cities.19$resp))] <- "Other"

# --- CORPORATIONS
# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Corporations/Corporations Responses/Climate Change'))

# --- Load the 2020 data
df_corps.20 <- read.csv('2020_Full_Climate_Change_Dataset.csv') %>% 
  dplyr::select('id' = account_number, 'org' = organization, 
                'qstn' = question_number, 'qstname' = question_unique_reference,
                'coln' = column_number, 'colname' = column_name,
                'rown' = row_number, 'rowname' = row_name, 'resp' = response_value)
# Recode to UTF-8
temp_vec <- df_corps.20$resp
Encoding(temp_vec) <- "UTF-8"
df_corps.20$resp <- temp_vec
temp_vec <- df_corps.20$colname
Encoding(temp_vec) <- "UTF-8"
df_corps.20$colname <- temp_vec
# Recode 'Other' category
df_corps.20$resp[grepl("^Other, please specify", as.character(df_corps.20$resp))] <- "Other"

# --- Load the 2019 data
df_corps.19 <- read.csv('2019_Full_Climate_Change_Dataset.csv') %>% 
  dplyr::select('id' = account_number, 'org' = organization, 
                'qstn' = question_number, 'qstname' = question_unique_reference,
                'coln' = column_number, 'colname' = column_name,
                'rown' = row_number, 'rowname' = row_name, 'resp' = response_value)
# Recode to UTF-8
temp_vec <- df_corps.19$resp
Encoding(temp_vec) <- "UTF-8"
df_corps.19$resp <- temp_vec
temp_vec <- df_corps.19$colname
Encoding(temp_vec) <- "UTF-8"
df_corps.19$colname <- temp_vec
# Recode 'Other' category
df_corps.19$resp[grepl("^Other, please specify", as.character(df_corps.19$resp))] <- "Other"



############## ~~~ CITIES ~~~ ###################################################################

############## Q2.2: NLP ##################################################################################

### Preprocessing
df_q2_2 <- df_cities.20 %>% filter(qstn == "2.2")
df_q2_2.wide <- dcast(df_q2_2, id + rown ~ coln, value.var="resp")
df_q2_2.wide <- df_q2_2.wide %>%
  rename("factor"="1", "support_challenges"="2", "degree"="3", "text"="4")
df_q2_2.wide <- df_q2_2.wide %>% filter(text!="", support_challenges!="")

### Translate to English
# Add language of answers
city_lang <- df_cities.20 %>%
  filter(qstname == "What language are you submitting your response in?") %>%
  dplyr::select(id, resp) %>% mutate(resp=dplyr::recode(resp, "Chinese"='zh-CN', "English"="en", "French"="fr",
                                                        "Korean"="ko", "Other (only applies to free text): italiano"="it",
                                                        "Portuguese"="pt", "Spanish"="es")) %>%
  rename(lang=resp)
df_q2_2.wide <- df_q2_2.wide %>% left_join(city_lang)
# Split by English and Non-English answers
df_q2_2.wide.en <- df_q2_2.wide %>% filter(lang=="en")
df_q2_2.wide.nonen <- df_q2_2.wide %>% filter(lang!="en")

# Translate
df_list <- list()
unique_langs <- unique(df_q2_2.wide.nonen$lang)
for (i in (1:length(unique_langs))){
  
  df_temp <- df_q2_2.wide.nonen %>% filter(lang==unique_langs[i])
  df_temp <- translate(dataset = df_temp,
                       content.field = 'text',
                       google.api.key = 'AIzaSyCyADH7ABEw5_KB8e_pG2e9Qc6Aa720Jmg',
                       source.lang = unique_langs[i],
                       target.lang = 'en')
  df_list[[i]] <- df_temp
  print(unique_langs[i])
  
}
df_q2_2.wide.nonen <- do.call(rbind, df_list)
df_q2_2.wide.nonen <- df_q2_2.wide.nonen %>% 
  dplyr::select(-c(text)) %>%
  rename(text=translatedContent)
df_q2_2.wide <- rbind(df_q2_2.wide.nonen, df_q2_2.wide.en)


### Preprocessing with cleanNLP
# --- > !!! DIFFERENT CODE FOR KAGGLE NOTEBOOK, see "cdp_draft_r"

# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = tolower(df_q2_2.wide$text))
df_q2_2.wide$doc_id <- annotation$document$doc_id
# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
                          filter(!(lemma %in% stopwords_vec),
                                 !(upos %in% c("DET", "PUNCT")),
                                 (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
                          dplyr::select(doc_id, lemma) %>%
                          group_by(doc_id) %>%
                          summarise_at(vars(lemma), funs(paste(., collapse = ' ')))
df_q2_2.wide <- df_q2_2.wide %>% right_join(df_text_preprocessing)

# cross_temp <- df_q2_2.wide %>%
#               group_by(factor) %>%
#               summarise(n=n(), supp=sum(support_challenges=="Supports")) %>%
#               mutate(prop_supp=supp/n)
# 
# 
# df_short <- df_q2_2.wide %>% filter(factor=="Government capacity")
# df_short <- df_q2_2.wide %>% filter(factor=="Land use planning")

### Preprocessing with quanteda
corpus_q2_2 <- corpus(df_q2_2.wide$lemma,
       docvars=df_q2_2.wide %>% dplyr::select(-c(text, lemma)))
dfmat_q2_2 <- tokens(corpus_q2_2) %>%
  tokens_ngrams(n = c(1,2)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)
ncol(dfmat_q2_2)

# GML
lasso <- glmnet(x = dfmat_q2_2,
                y = as.integer(dfmat_q2_2@docvars$support_challenges == "Supports"),
                alpha = 1,
                family = "binomial")
index_best <- which(lasso$lambda == min(lasso$lambda))
beta <- lasso$beta[, index_best]
head(sort(beta, decreasing = T), 10)
head(sort(beta, decreasing = F), 10)



############## >>>>> Q2.0b - 2020 #########################################################################

# TODO:
# 1. Look for .pdf files in the page, TOO COMLICATED
# 2. Same questions in 2019 and 2018

### --------- Q2.0b

df_q2_0b <- df_cities.20 %>% filter(qstn == "2.0b")
df_q2_0b.link <- df_q2_0b %>%
  filter(colname == "Web link",
         !(resp %in% c("Question not applicable", ""))) %>%
  dplyr::select(id, resp)
# Extract the links
temp_vec <- sapply(df_q2_0b.link$resp, function(x) str_extract_all(x, "(?<=^|\\s)http[^\\s]+"))
temp_vec <- as.data.frame(sapply(temp_vec, function(x) paste(x, collapse = ' ')))
df_q2_0b.link$resp <- temp_vec[,1]
df_q2_0b.link <- df_q2_0b.link %>% filter(resp!="")
df_q2_0b.link <- df_q2_0b.link %>% separate(resp, paste0("link", as.character(1:9)),
                                            sep=" ")
df_q2_0b.link <- df_q2_0b.link[,colSums(is.na(df_q2_0b.link)) != nrow(df_q2_0b.link)]
# Transform to long-format: id-link as observation
df_q2_0b.link.long <- melt(df_q2_0b.link, id.vars=c("id")) %>%
  filter(!is.na(value)) %>% rename(link_num=variable, link=value) %>%
  mutate(link_num=substr(link_num, 5,5),
         id_link_num=paste(id, link_num, sep="_")) %>% 
  slice(-416) # Remove city with a large book as a report

### Download reports
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2020/q2_0b")
oldw <- getOption("warn")
options(warn = -1)
for (i in (1:nrow(df_q2_0b.link.long))) {
  tryCatch(download.file(df_q2_0b.link.long$link[i],
                         destfile = paste0(df_q2_0b.link.long$id_link_num[i], ".pdf"),
                         mode = "wb", quiet = T), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  Sys.sleep(1)
}
options(warn = oldw)

### Load the data from separate pdfs
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2020/q2_0b")
pdf_files <- list.files(pattern = "pdf$")
oldw <- getOption("warn")
options(warn = -1)
pdf_texts <- list()
j <- 1
for (i in (1:length(pdf_files))) {
  tryCatch(pdf_texts[[i]] <- pdf_text(pdf_files[i]), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  # Sys.sleep(1)
}
pdf_files <- pdf_files[!unlist(lapply(pdf_texts, is.null))]
pdf_texts <- pdf_texts[!unlist(lapply(pdf_texts, is.null))]

df_texts <- as.data.frame(sapply(pdf_texts, function(x) paste(x, collapse = '---PAGE-BREAK---')))
colnames(df_texts) <- "text"
df_texts$id_link_num <- substr(pdf_files, 1, (nchar(pdf_files)-4))

# Select only reports that are written in English
# TODO: Translate non-english reports
df_texts$lang <- detect_language(df_texts$text)
df_texts.en <- df_texts %>% filter(lang=="en") %>% dplyr::select(-c(lang))
# df_texts.nonen <- df_texts %>% filter(lang!="en")

# TO LONL
df_texts.en <- df_texts.en %>% separate(text, paste0("page", as.character(1:500)), sep="---PAGE-BREAK---")
# Transform to long-format: id-link as observation
df_texts.en.long <- melt(df_texts.en, id.vars=c("id_link_num")) %>%
  filter(!is.na(value)) %>% rename(page=variable, text=value) %>%
  mutate(page=as.character(page),
         page=substr(page, 5, nchar(page)),
         # text=tolower(text), # CHANGE FOR NER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         id=substr(id_link_num, 1, (nchar(id_link_num)-2))) 

### NLP part
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = df_texts.en.long, verbose = 10)
# Add City ID
df_temp <- annotation$token
df_temp <- df_temp %>% left_join(annotation$document %>% dplyr::select(id, doc_id))
annotation$token <- df_temp

# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(id, lemma) %>%
  group_by(id) %>%
  summarise_at(vars(lemma), funs(paste(., collapse = ' ')))

# Create Corpus and Document term matrix
corpus_q2_0b <- corpus(df_text_preprocessing$lemma,
                       docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q2_0b <- tokens(corpus_q2_0b) %>%
  tokens_ngrams(n = c(1)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)

### 1. STM 
stm_model <- stm(documents = dfm_q2_0b,
                 K = 5, max.em.its = 75, init.type = "Spectral")
labelTopics(stm_model)
plot(stm_model, type = "summary")



############### >>>>> Q3.0 - 2020 #########################################################################

### NEW
df_q3_0 <- df_cities.20 %>% filter(qstn == "3.0")
df_q3_0.link <- df_q3_0 %>%
  filter(colname == "Web link",
         !(resp %in% c("Question not applicable", ""))) %>%
  dplyr::select(id, resp)
# Extract the links
temp_vec <- sapply(df_q3_0.link$resp, function(x) str_extract_all(x, "(?<=^|\\s)http[^\\s]+"))
temp_vec <- as.data.frame(sapply(temp_vec, function(x) paste(x, collapse = ' ')))
df_q3_0.link$resp <- temp_vec[,1]
df_q3_0.link <- df_q3_0.link %>% filter(resp!="")
df_q3_0.link <- df_q3_0.link %>% separate(resp, paste0("link", as.character(1:9)),
                                            sep=" ")
df_q3_0.link <- df_q3_0.link[,colSums(is.na(df_q3_0.link)) != nrow(df_q3_0.link)]
# Transform to long-format: id-link as observation
df_q3_0.link.long <- melt(df_q3_0.link, id.vars=c("id")) %>%
  filter(!is.na(value)) %>% rename(link_num=variable, link=value) %>%
  mutate(link_num=substr(link_num, 5,5),
         id_link_num=paste(id, link_num, sep="_"))

### Download reports
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2020/q3_0")
oldw <- getOption("warn")
options(warn = -1)
for (i in (1:nrow(df_q3_0.link.long))) {
  tryCatch(download.file(df_q3_0.link.long$link[i],
                         destfile = paste0(df_q3_0.link.long$id_link_num[i], ".pdf"),
                         mode = "wb", quiet = T), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  Sys.sleep(0.5)
}
options(warn = oldw)

### Load the data from separate pdfs
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2020/q3_0")
pdf_files <- list.files(pattern = "pdf$")
oldw <- getOption("warn")
options(warn = -1)
pdf_texts <- list()
# i_vec <- c()
for (i in (1:length(pdf_files))) {
  tryCatch(pdf_texts[[i]] <- pdf_text(pdf_files[i]), 
           # error = function(e) print(paste(as.character(i), ' error ~~~~~')))
           error = function(e) pdf_texts[[i]] <- NULL)
  print(i)
  # i_vec <- c(i_vec, i)
}
# pdf_files <- pdf_files[!unlist(lapply(pdf_texts, is.null))] TODO: FIX THE BUG
pdf_texts <- pdf_texts[!unlist(lapply(pdf_texts, is.null))]


df_texts <- as.data.frame(sapply(pdf_texts, function(x) paste(x, collapse = '---PAGE-BREAK---')))
colnames(df_texts) <- "text"

df_texts$id <- as.character(1:nrow(df_texts)) # TODO: FIX THE BUG

# Select only reports that are written in English
# TODO: Translate non-english reports
df_texts$lang <- detect_language(df_texts$text)
df_texts.en <- df_texts %>% filter(lang=="en") %>% dplyr::select(-c(lang))
# df_texts.nonen <- df_texts %>% filter(lang!="en")

# TO LONLG
df_texts.en <- df_texts.en %>% separate(text, paste0("page", as.character(1:500)), sep="---PAGE-BREAK---")
# Transform to long-format: id-page as observation
df_texts.en.long <- melt(df_texts.en, id.vars=c("id")) %>%
  filter(!is.na(value)) %>% rename(page=variable, text=value) %>%
  mutate(page=as.character(page),
         page=substr(page, 5, nchar(page)))

# REMOVE LONG PAGE
df_texts.en.long$len <- unlist(lapply(df_texts.en.long$text, nchar))
df_texts.en.long <- df_texts.en.long %>% filter(len<1e6)

### NLP part
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = df_texts.en.long, verbose = 10)
# Add City ID
df_temp <- annotation$token
df_temp <- df_temp %>% left_join(annotation$document %>% dplyr::select(id, doc_id))
annotation$token <- df_temp

# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT", "PROPN")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(id, lemma) %>%
  group_by(id) %>%
  summarise_at(vars(lemma), funs(paste(., collapse = ' ')))

# Create Corpus and Document term matrix
corpus_q3_0 <- corpus(df_text_preprocessing$lemma,
                       docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q3_0 <- tokens(corpus_q3_0) %>%
  tokens_ngrams(n = c(1, 2)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 20)
ncol(dfm_q3_0)
nrow(dfm_q3_0)

# Hierarchical clustering
tstat_dist <- as.dist(textstat_dist(dfm_q3_0))
clust <- hclust(tstat_dist, method = "ward.D")
plot(clust, xlab = "Distance", ylab = NULL)
dfm_q3_0@docvars$cluster <- cutree(clust, k = 2)

set.seed(132)
dfm_q3_0_grouped <- dfm(dfm_q3_0,  groups = "cluster")
# dev.new(width = 1000, height = 1000, unit = "px")
textplot_wordcloud(dfm_q3_0_grouped, comparison = T, max_words = 50, ordered_color=T,
                   color=c("forestgreen", "red"), fixed_aspect=T, min_size = 1, max_size = 3)



############## >>>>> Q2.0b - 2019 ################################################################

# TODO:
# 1. Add Categorical variables to STM, Year of publication/Adoption, VUNERABLE POPULATIONS
# 2. Combine Reports from 2018 and 2019


### --------- Q2.0b

df_q2_0b <- df_cities.19 %>% filter(qstn == "2.0b")
df_q2_0b.link <- df_q2_0b %>%
  filter(colname == "Web link",
         !(resp %in% c("Question not applicable", ""))) %>%
  dplyr::select(id, resp)
# Extract the links
temp_vec <- sapply(df_q2_0b.link$resp, function(x) str_extract_all(x, "(?<=^|\\s)http[^\\s]+"))
temp_vec <- as.data.frame(sapply(temp_vec, function(x) paste(x, collapse = ' ')))
df_q2_0b.link$resp <- temp_vec[,1]
df_q2_0b.link <- df_q2_0b.link %>% filter(resp!="")
df_q2_0b.link <- df_q2_0b.link %>% separate(resp, paste0("link", as.character(1:9)),
                                            sep=" ")
df_q2_0b.link <- df_q2_0b.link[,colSums(is.na(df_q2_0b.link)) != nrow(df_q2_0b.link)]
# Transform to long-format: id-link as observation
df_q2_0b.link.long <- melt(df_q2_0b.link, id.vars=c("id")) %>%
  filter(!is.na(value)) %>% rename(link_num=variable, link=value) %>%
  mutate(link_num=substr(link_num, 5,5),
         id_link_num=paste(id, link_num, sep="_"))

### Download reports
#setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2019/q2_0b")
#oldw <- getOption("warn")
#options(warn = -1)
#for (i in (1:nrow(df_q2_0b.link.long))) {
#  tryCatch(download.file(df_q2_0b.link.long$link[i],
#                         destfile = paste0(df_q2_0b.link.long$id_link_num[i], ".pdf"),
#                         mode = "wb", quiet = T), 
#           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
#  print(i)
#  Sys.sleep(0.5)
#}
#options(warn = oldw)

### Load the data from separate pdfs
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2019/q2_0b")
pdf_files <- list.files(pattern = "pdf$")
oldw <- getOption("warn")
options(warn = -1)
pdf_texts <- list()
for (i in (1:length(pdf_files))) {
  tryCatch(pdf_texts[[i]] <- pdf_text(pdf_files[i]), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  # Sys.sleep(1)
}
pdf_files <- pdf_files[!unlist(lapply(pdf_texts, is.null))]
pdf_texts <- pdf_texts[!unlist(lapply(pdf_texts, is.null))]

df_texts <- as.data.frame(sapply(pdf_texts, function(x) paste(x, collapse = '---PAGE-BREAK---')))
colnames(df_texts) <- "text"
df_texts$id_link_num <- substr(pdf_files, 1, (nchar(pdf_files)-4))

# Select only reports that are written in English
df_texts$lang <- detect_language(df_texts$text)
df_texts.en <- df_texts %>% filter(lang=="en") %>% dplyr::select(-c(lang))
# TO LONL
df_texts.en <- df_texts.en %>% separate(text, paste0("page", as.character(1:500)), sep="---PAGE-BREAK---")
# Transform to long-format: id-link as observation
df_texts.en.long <- melt(df_texts.en, id.vars=c("id_link_num")) %>%
  filter(!is.na(value)) %>% rename(page=variable, text=value) %>%
  mutate(page=as.character(page),
         page=substr(page, 5, nchar(page)),
         # text=tolower(text), # CHANGE FOR NER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         id=substr(id_link_num, 1, (nchar(id_link_num)-2))) 


### NLP part
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = df_texts.en.long, verbose = 10)
# Add City ID
df_temp <- annotation$token
df_temp <- df_temp %>% left_join(annotation$document %>% dplyr::select(id, doc_id))
annotation$token <- df_temp

# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")

df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT", "PROPN")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(doc_id, lemma, id) %>%
  # group_by(id) %>%
  group_by(doc_id) %>% # PAGE AS THE UNIT OF OBSERVATION <<<<<<<<<<<<<<<<<<<<<<<<
  summarise_at(vars(lemma, id), funs(paste(., collapse = ' ')))
df_text_preprocessing$id <- unlist(lapply(df_text_preprocessing$id, function(x) str_split(x, " ")[[1]][1]))

# Remove pages with low number of characters
df_text_preprocessing$len <- unlist(lapply(df_text_preprocessing$lemma, nchar))
df_text_preprocessing <- df_text_preprocessing %>% filter(len>200)

# Save the preprocessed file
setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
# saveRDS(df_text_preprocessing, "df_text_preprocessing_q2_0b_2019")
df_text_preprocessing <- readRDS("df_text_preprocessing_q2_0b_2019")

# Add categorical variables to text
df_q2_0b.vuln <- df_q2_0b %>% dplyr::filter(coln == 8, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=as.character(id)) %>% rename(vulnerable_population=resp)
df_q2_0b.year <- df_q2_0b %>% dplyr::filter(coln == 2, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=as.character(id)) %>% rename(adoption_year=resp)
df_text_preprocessing <- df_text_preprocessing%>% data.frame() %>% 
  left_join(df_q2_0b.year, by="id") %>%
  left_join(df_q2_0b.vuln, by="id") %>%
  distinct(doc_id, .keep_all = T)
df_text_preprocessing <- df_text_preprocessing %>%
  mutate(adoption_year=ifelse(adoption_year %in% c("2016", "2017", "2018", "2019"),
                              adoption_year, "2015 and earlier"),
         adoption_year=dplyr::recode(adoption_year,
                                     "2016"="2016-2017",
                                     "2017"="2016-2017",
                                     "2018"="2018-2019",
                                     "2019"="2018-2019"))
# Remove documents with missing categorical variables
df_text_preprocessing <- df_text_preprocessing %>% filter(!is.na(vulnerable_population))

# Create Corpus and Document term matrix
corpus_q2_0b <- corpus(df_text_preprocessing$lemma,
                      docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q2_0b <- tokens(corpus_q2_0b) %>%
  tokens_ngrams(n = c(1)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 20)
ncol(dfm_q2_0b)

### --- STM 

# Model 1. Base
stm_model.1 <- stm(documents = dfm_q2_0b,
                 K = 0, max.em.its = 25, init.type = "Spectral")
labelTopics(stm_model.1)
plot(stm_model.1, type = "summary", n = 5, xlim = c(0,1))
mod.out.corr <- topicCorr(stm_model.1)
plot(mod.out.corr)

# Model 2. Base + Categorical variables
stm_model.2 <- stm(documents = dfm_q2_0b,
                   prevalence =~ vulnerable_population + adoption_year,
                   K = 0, max.em.its = 25, init.type = "Spectral")
topics_number <- 66
labelTopics(stm_model.2)
plot(stm_model.2, type = "summary", n = 5, xlim = c(0,1))
mod.out.corr <- topicCorr(stm_model.2)
plot(mod.out.corr)


# Estimate the effect of adoption_year on topics
prep.1 <- estimateEffect(1:topics_number ~ adoption_year, stm_model.2,
                         uncertainty = "Global",
                         meta = dfm_q2_0b@docvars)
summary(prep.1, topics=c(1:5))
plot(prep.1, covariate = "adoption_year", topics = c(1:topics_number),
     model = stm_model.2, method = "difference",
     cov.value1 = "2018-2019", cov.value2 = "2015 and earlier",
     xlab = "2015 and earlier < ................... > 2018-2019",
     main = "Topics for Based on the years",
     xlim = c(-.1, .1),
     labeltype = "custom")
labelTopics(stm_model.2, topics = c(4, 12)) # 2018-2019
labelTopics(stm_model.2, topics = c(52, 15, 6)) # 201 and earlier


# Estimate the effect of vulnerable_population on topics
prep.2 <- estimateEffect(1:topics_number ~ vulnerable_population, stm_model.2,
                         uncertainty = "Global",
                         meta = dfm_q2_0b@docvars)
summary(prep.2, topics=c(1:5))
plot(prep.2, covariate = "vulnerable_population", topics = c(1:topics_number),
     model = stm_model.2, method = "difference",
     cov.value1 = "Yes", cov.value2 = "No",
     xlab = "No Vulnerable Population < ................... > Yes Vulnerable Population",
     main = "Topics for Vulnerable Population",
     xlim = c(-.1, .1),
     labeltype = "custom")
labelTopics(stm_model.2, topics = c(6, 14, 24))






############## >>>>> # Q2.0b 2019, 2020 ################################################################

# TODO:
# 1. Add Categorical variables to STM, Year of publication/Adoption, VUNERABLE POPULATIONS
# 2. Combine Reports from 2018 and 2019
# 3. Split by pages and than use  id as a categorical variable?
# 4. Clustering of pages?
# 5. Primary author of assessment

### Load the data from separate pdfs ------------- 2019
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2019/q2_0b")
pdf_files <- list.files(pattern = "pdf$")
oldw <- getOption("warn")
options(warn = -1)
pdf_texts <- list()
for (i in (1:length(pdf_files))) {
  tryCatch(pdf_texts[[i]] <- pdf_text(pdf_files[i]), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  # Sys.sleep(1)
}
pdf_files <- pdf_files[!unlist(lapply(pdf_texts, is.null))]
pdf_texts <- pdf_texts[!unlist(lapply(pdf_texts, is.null))]

df_texts <- as.data.frame(sapply(pdf_texts, function(x) paste(x, collapse = '---PAGE-BREAK---'))) # ---PAGE-BREAK---
colnames(df_texts) <- "text"
df_texts$id_link_num <- substr(pdf_files, 1, (nchar(pdf_files)-4))

# Select only reports that are written in English
df_texts$lang <- detect_language(df_texts$text)
df_texts.en.2019 <- df_texts %>% filter(lang=="en") %>% dplyr::select(-c(lang))
df_texts.en.2019$year <- "2019"

### Load the data from separate pdfs ------------- 2020
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2020/q2_0b")
pdf_files <- list.files(pattern = "pdf$")
oldw <- getOption("warn")
options(warn = -1)
pdf_texts <- list()
for (i in (1:length(pdf_files))) {
  tryCatch(pdf_texts[[i]] <- pdf_text(pdf_files[i]), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  # Sys.sleep(1)
}
pdf_files <- pdf_files[!unlist(lapply(pdf_texts, is.null))]
pdf_texts <- pdf_texts[!unlist(lapply(pdf_texts, is.null))]

df_texts <- as.data.frame(sapply(pdf_texts, function(x) paste(x, collapse = '---PAGE-BREAK---'))) # ---PAGE-BREAK---
colnames(df_texts) <- "text"
df_texts$id_link_num <- substr(pdf_files, 1, (nchar(pdf_files)-4))

# Select only reports that are written in English
df_texts$lang <- detect_language(df_texts$text)
df_texts.en.2020 <- df_texts %>% filter(lang=="en") %>% dplyr::select(-c(lang))
df_texts.en.2020$year <- "2020"


# Merge together ------------- 2020 & 2019
df_texts.en <- rbind(df_texts.en.2019, df_texts.en.2020)
df_texts.en <- df_texts.en %>% distinct(text, .keep_all = T)
df_texts.en <- df_texts.en %>%
  mutate(id_link_num=substr(id_link_num, 1, (nchar(id_link_num)-2)),
         id=paste(id_link_num, year, sep  = "_")) %>%
  dplyr::select(-c(id_link_num))

# TO LONL
df_texts.en <- df_texts.en %>% separate(text, paste0("page", as.character(1:500)), sep="---PAGE-BREAK---")
# Transform to long-format: id-link as observation
df_texts.en.long <- melt(df_texts.en, id.vars=c("id")) %>%
  filter(!is.na(value)) %>% rename(page=variable, text=value) %>%
  mutate(page=as.character(page),
         page=substr(page, 5, nchar(page)),
         # text=tolower(text), # CHANGE FOR NER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ) 

### NLP part
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = df_texts.en.long, verbose = 100)
# Add City ID
df_temp <- annotation$token
df_temp <- df_temp %>% left_join(annotation$document %>% dplyr::select(id, doc_id))
annotation$token <- df_temp
# Save the preprocessed file
setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
# saveRDS(annotation, "annotation_q2_0b_2019_2020")

# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT", "PROPN")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(doc_id, id, lemma) %>%
  group_by(id) %>% # <<<<<<<<<<<<<<<<<<<<<<<<< BY CITY
  # group_by(doc_id) %>% # <<<<<<<<<<<<<<<<<<<<<<<<< BY PAGE
  summarise_at(vars(lemma), funs(paste(., collapse = ' '))) # add id to vars
# df_text_preprocessing$id <- unlist(lapply(df_text_preprocessing$id, function(x) str_split(x, " ")[[1]][1]))


# saveRDS(df_text_preprocessing, "df_text_preprocessing_q2_0b_2019_2020_obscities")
# saveRDS(df_text_preprocessing, "df_text_preprocessing_q2_0b_2019_2020_obspages")

### --- STM ----------------------------------------------------------------------

# Save the preprocessed file
setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
df_text_preprocessing <- readRDS("df_text_preprocessing_q2_0b_2019_2020_obscities")
df_text_preprocessing <- readRDS("df_text_preprocessing_q2_0b_2019_2020_obspages")

# Add categorical variables to text
df_q2_0b <- df_cities.20 %>% filter(qstn == "2.0b")
df_q2_0b.vuln <- df_q2_0b %>% dplyr::filter(coln == 7, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=as.character(id)) %>% rename(vulnerable_population=resp)
df_q2_0b.year <- df_q2_0b %>% dplyr::filter(coln == 3, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=as.character(id)) %>% rename(adoption_year=resp)
df_text_preprocessing <- df_text_preprocessing %>% data.frame() %>% 
  left_join(df_q2_0b.year, by="id") %>%
  left_join(df_q2_0b.vuln, by="id") %>%
  distinct(doc_id,  .keep_all = T)

# Remove documents with missing categorical variables
df_text_preprocessing <- df_text_preprocessing %>% filter(!is.na(adoption_year))
df_text_preprocessing <- df_text_preprocessing %>%
  mutate(adoption_year=ifelse(adoption_year %in% c("2016", "2017", "2018", "2019"),
                              adoption_year, "2015 and earlier"),
         adoption_year=dplyr::recode(adoption_year,
                                     "2016"="2016-2017",
                                     "2017"="2016-2017",
                                     "2018"="2018-2019",
                                     "2019"="2018-2019"))
df_text_preprocessing <- df_text_preprocessing %>%
  mutate(vulnerable_population = vulnerable_population %>% replace_na("No"))

# Remove pages with low number of characters
df_text_preprocessing$len <- unlist(lapply(df_text_preprocessing$lemma, nchar))
df_text_preprocessing <- df_text_preprocessing %>% filter(len>200)

# Create Corpus and Document term matrix
corpus_q2_0b <- corpus(df_text_preprocessing$lemma,
                       docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q2_0b <- tokens(corpus_q2_0b) %>%
  tokens_ngrams(n = c(1)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 20)
ncol(dfm_q2_0b)
nrow(dfm_q2_0b)


# Model 0. Base <<<<<<<<<<<<<<<<<<<<<<<< OK with k = 75
stm_model.0 <- stm(documents = dfm_q2_0b,
                   K = 0, max.em.its = 50, init.type = "Spectral") # K = 75
topics_number <- 75
labelTopics(stm_model.0)
plot(stm_model.0, type = "summary", n = 5, xlim = c(0,1))
mod.out.corr <- topicCorr(stm_model.0)
plot(mod.out.corr)

# Model 1. Base + Categorical variables
K <- 50
stm_model.1 <- stm(documents = dfm_q2_0b,
                   prevalence =~ vulnerable_population + adoption_year,
                   K = K, max.em.its = 50, init.type = "Spectral")
labelTopics(stm_model.1)
plot(stm_model.1, type = "summary", n = 5, xlim = c(0,1))
mod.out.corr <- topicCorr(stm_model.1)
plot(mod.out.corr)

# Estimate the effect of adoption_year on topics
prep.1 <- estimateEffect(1:K ~ adoption_year, stm_model.1,
                         uncertainty = "Global",
                         meta = dfm_q2_0b@docvars)
summary(prep.1, topics=c(1:K))
plot(prep.1, covariate = "adoption_year", topics = c(1:K),
     model = stm_model.1, method = "difference",
     cov.value1 = "2018-2019", cov.value2 = "2015 and earlier",
     xlab = "2015 and earlier < ................... > 2018-2019",
     main = "Topics for Based on the years",
     xlim = c(-.1, .1),
     labeltype = "custom")
labelTopics(stm_model.1, topics = c(14, 9, 3)) # 2018-2019
labelTopics(stm_model.1, topics = c(17)) # 2015 and earlier

# Estimate the effect of vulnerable_population on topics
prep.2 <- estimateEffect(1:K ~ vulnerable_population, stm_model.1,
                         uncertainty = "Global",
                         meta = dfm_q2_0b@docvars)
summary(prep.2, topics=c(1:K))
plot(prep.2, covariate = "vulnerable_population", topics = c(1:K),
     model = stm_model.1, method = "difference",
     cov.value1 = "Yes", cov.value2 = "No",
     xlab = "No Vulnerable Population < ................... > Yes Vulnerable Population",
     main = "Topics for Vulnerable Population",
     xlim = c(-.2, .2),
     labeltype = "custom")
labelTopics(stm_model.1, topics = c(17, 15))


### --- CLUSTERING ----------------------------------------------------------------------

# Save the preprocessed file
setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
df_text_preprocessing <- readRDS("df_text_preprocessing_q2_0b_2019_2020_obscities")

# Create Corpus and Document term matrix
corpus_q2_0b <- corpus(df_text_preprocessing$lemma,
                       docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q2_0b <- tokens(corpus_q2_0b) %>%
  tokens_ngrams(n = c(1, 2, 3)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 20)
ncol(dfm_q2_0b)
nrow(dfm_q2_0b)

# Hierarchical clustering
tstat_dist <- as.dist(textstat_dist(dfm_q2_0b))
clust <- hclust(tstat_dist, method = "ward.D")
plot(clust, xlab = "Distance", ylab = NULL)
dfm_q2_0b@docvars$cluster <- cutree(clust, k = 3)

set.seed(132)
textplot_wordcloud(dfm_q2_0b, max_words = 100)
textplot_wordcloud(dfm_q2_0b[dfm_q2_0b$cluster == 1,], max_words = 100)
textplot_wordcloud(dfm_q2_0b[dfm_q2_0b$cluster == 2,], max_words = 100)
textplot_wordcloud(dfm_q2_0b[dfm_q2_0b$cluster == 3,], max_words = 100)


dfm_q2_0b_grouped <- dfm(dfm_q2_0b,  groups = "cluster")
# dev.new(width = 1000, height = 1000, unit = "px")
textplot_wordcloud(dfm_q2_0b_grouped, comparison = T, max_words = 100, ordered_color=T,
                   color=c("black", "forestgreen", "red"), fixed_aspect=T, min_size = 1, max_size = 2)

# dfm_q2_0b_grouped <- dfm(dfm_q2_0b,  groups = "author")
# # dev.new(width = 1000, height = 1000, unit = "px")
# textplot_wordcloud(dfm_q2_0b_grouped, comparison = T, max_words = 100, ordered_color=T,
#                    color=c("forestgreen", "red"), fixed_aspect=T, min_size = 0.5)


### --- LEXICAL DIVERSITY ----------------------------------------------------------------------

# Save the preprocessed file
setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
df_text_preprocessing <- readRDS("df_text_preprocessing_q2_0b_2019_2020_obscities")

tstat_lexdiv <- textstat_lexdiv(dfm_q2_0b, "all")

# TTR
# ggplot(tstat_lexdiv, aes(x = reorder(document, TTR),
#                          y = TTR)) + 
#   geom_point(size=2, color=adjustcolor("brown2", alpha.f = 0.8)) +
#   coord_flip() + theme_minimal()

# Carroll's Corrected TTR
ggplot(tstat_lexdiv, aes(x = reorder(document, -CTTR),
                         y = CTTR)) + 
  geom_point(size=2, color=adjustcolor("brown2", alpha.f = 0.8)) +
  coord_flip() + theme_minimal()


### --- RELATIVE FREQUENCY ANALYSIS ------------------------------------------------------------

# Load the preprocessed file
setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
df_text_preprocessing <- readRDS("df_text_preprocessing_q2_0b_2019_2020_obscities")

# Add categorical variables to text
# 2020
df_q2_0b.20 <- df_cities.20 %>% filter(qstn == "2.0b")
df_q2_0b.vuln.20 <- df_q2_0b.20 %>% dplyr::filter(coln == 7, rown==1, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=paste0(as.character(id), "_2020")) %>% rename(vulnerable_population=resp)
df_q2_0b.year.20 <- df_q2_0b.20 %>% dplyr::filter(coln == 3, rown==1, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=paste0(as.character(id), "_2020")) %>% rename(adoption_year=resp)
df_q2_0b.author.20 <- df_q2_0b.20 %>% dplyr::filter(coln == 6, rown==1, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=paste0(as.character(id), "_2020")) %>% rename(author=resp)
# 2019
df_q2_0b.19 <- df_cities.19 %>% filter(qstn == "2.0b")
df_q2_0b.vuln.19 <- df_q2_0b.19 %>% dplyr::filter(coln == 8, rown==1, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=paste0(as.character(id), "_2019")) %>% rename(vulnerable_population=resp)
df_q2_0b.year.19 <- df_q2_0b.19 %>% dplyr::filter(coln == 2, rown==1, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=paste0(as.character(id), "_2019")) %>% rename(adoption_year=resp)
df_q2_0b.author.19 <- df_q2_0b.19 %>% dplyr::filter(coln == 7, rown==1, resp != "") %>% dplyr::select(id, resp) %>%
  mutate(id=paste0(as.character(id), "_2019")) %>% rename(author=resp)
# Merge
df_q2_0b.year <- rbind(df_q2_0b.year.19, df_q2_0b.year.20)
df_q2_0b.vuln <- rbind(df_q2_0b.vuln.19, df_q2_0b.vuln.20)
df_q2_0b.author <- rbind(df_q2_0b.author.19, df_q2_0b.author.20)
# Add to the dataset
df_text_preprocessing <- df_text_preprocessing %>% data.frame() %>% 
  left_join(df_q2_0b.year, by="id") %>%
  left_join(df_q2_0b.vuln, by="id") %>%
  left_join(df_q2_0b.author, by="id")

# adoption_year preprocessing
df_text_preprocessing <- df_text_preprocessing %>% filter(!is.na(adoption_year))
df_text_preprocessing <- df_text_preprocessing %>%
  mutate(adoption_year=ifelse(adoption_year %in% c("2016", "2017", "2018", "2019"),
                              "2016-2019", "2015 and earlier"))

# vulnerable_population preprocessing
df_text_preprocessing <- df_text_preprocessing %>%
  mutate(vulnerable_population = vulnerable_population %>% replace_na("No"))

# author preprocessing
df_text_preprocessing <- df_text_preprocessing %>%
  mutate(author=ifelse(author %in% c("Consultant", "Other", "International organization"),
                       "Internal", "External"))

# Create Corpus and Document term matrix
corpus_q2_0b <- corpus(df_text_preprocessing$lemma,
                       docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q2_0b <- tokens(corpus_q2_0b) %>%
  tokens_ngrams(n = c(1, 2, 3)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 20)
ncol(dfm_q2_0b)
nrow(dfm_q2_0b)


table(dfm_q2_0b$adoption_year, dfm_q2_0b$author)

# Model 1: By Adoption Year
tstat_key.1 <- textstat_keyness(dfm_q2_0b, 
                                target = dfm_q2_0b$adoption_year=="2016-2019")
textplot_keyness(tstat_key.1)

# Model 2: By Vunerable Population
# tstat_key.2 <- textstat_keyness(dfm_q2_0b, 
#                                 target = dfm_q2_0b$vulnerable_population=="No")
# textplot_keyness(tstat_key.2)

# Model 3: By Author
tstat_key.3 <- textstat_keyness(dfm_q2_0b, 
                                target = dfm_q2_0b$author=="Internal")
textplot_keyness(tstat_key.3)


### --- NAMED ENTITIES  ------------------------------------------------------------

setwd("C:/Users/Artem/YandexDisk/CDP/data/nlp_files")
annotation <- readRDS("annotation_q2_0b_2019_2020")

df_entity <- annotation$entity
t1 <- as.data.frame(table(df_entity$entity_type))
t2 <- as.data.frame(table(df_entity %>% filter(entity_type=="LOC") %>% dplyr::select(entity)))
t3 <- as.data.frame(table(df_entity %>% filter(entity_type=="ORG") %>% dplyr::select(entity)))


### --- STM with CITIES as observations  ------------------------------------------------------------

# Model 1. Base + Categorical variables, K = 10
K <- 10
stm_model.1 <- stm(documents = dfm_q2_0b,
                   prevalence =~ vulnerable_population + adoption_year + author,
                   K = K, max.em.its = 50, init.type = "Spectral")
labelTopics(stm_model.1)
plot(stm_model.1, type = "summary", n = 5, xlim = c(0,1))
mod.out.corr <- topicCorr(stm_model.1)
plot(mod.out.corr)

# Model 1. Base + Categorical variables, K = 20
K <- 20
stm_model.1 <- stm(documents = dfm_q2_0b,
                   prevalence =~ vulnerable_population + adoption_year + author,
                   K = K, max.em.its = 50, init.type = "Spectral")
labelTopics(stm_model.1)

# Model 1. Base + Categorical variables, K = 0
stm_model.1 <- stm(documents = dfm_q2_0b,
                   prevalence =~ vulnerable_population + adoption_year + author,
                   K = 0, max.em.its = 50, init.type = "Spectral")
labelTopics(stm_model.1)
K <- 73
# Estimate the effect of adoption_year on topics
prep.1 <- estimateEffect(1:K ~ adoption_year, stm_model.1,
                         uncertainty = "Global",
                         meta = dfm_q2_0b@docvars)
summary(prep.1, topics=c(1:2))
plot(prep.1, covariate = "adoption_year", topics = c(1:K),
     model = stm_model.1, method = "difference",
     cov.value1 = "2016-2019", cov.value2 = "2015 and earlier",
     xlab = "2015 and earlier < ................... > 2016-2019",
     main = "Topics for Based on the years",
     xlim = c(-.3, .3),
     labeltype = "custom")

# Estimate the effect of author on topics
prep.2 <- estimateEffect(1:K ~ author, stm_model.1,
                         uncertainty = "Global",
                         meta = dfm_q2_0b@docvars)
summary(prep.2, topics=c(1:K))
plot(prep.2, covariate = "author", topics = c(1:K),
     model = stm_model.1, method = "difference",
     cov.value1 = "Internal", cov.value2 = "External",
     xlab = "External < ................... > Internal",
     main = "Topics Based on the Author",
     xlim = c(-.1, .1),
     labeltype = "custom")


labelTopics(stm_model.1, topics = c(14, 9, 3)) # 2018-2019
labelTopics(stm_model.1, topics = c(17)) # 2015 and earlier



############## Q6.0 : NLP  ####################################################################

### --- 2020
df_q6_0 <- df_cities.20 %>% filter(qstn == "6.0")
df_q6_0.wide <- dcast(df_q6_0, id + rown ~ coln, value.var="resp")
df_q6_0.wide <- df_q6_0.wide %>%
  rename("opportunity"="1", "text"="2") %>%
  filter(text!="")

# Translation
# Language of the city
city_lang <- df_cities.20 %>%
  filter(qstname == "What language are you submitting your response in?") %>%
  dplyr::select(id, resp) %>% mutate(resp=dplyr::recode(resp, "Chinese"='zh-CN', "English"="en", "French"="fr",
                                                        "Korean"="ko", "Other (only applies to free text): italiano"="it",
                                                        "Portuguese"="pt", "Spanish"="es")) %>%
  rename(lang=resp)

df_q6_0.wide <- df_q6_0.wide %>% left_join(city_lang)
df_q6_0.wide.en <- df_q6_0.wide %>% filter(lang=="en")
df_q6_0.wide.nonen <- df_q6_0.wide %>% filter(lang!="en")

df_list <- list()
unique_langs <- unique(df_q6_0.wide.nonen$lang)
for (i in (1:length(unique_langs))){
  
  df_temp <- df_q6_0.wide.nonen %>% filter(lang==unique_langs[i])
  df_temp <- translate(dataset = df_temp,
                       content.field = 'text',
                       google.api.key = 'AIzaSyCyADH7ABEw5_KB8e_pG2e9Qc6Aa720Jmg',
                       source.lang = unique_langs[i],
                       target.lang = 'en')
  df_list[[i]] <- df_temp
  print(unique_langs[i])
  
}

df_q6_0.wide.nonen <- do.call(rbind, df_list)
df_q6_0.wide.nonen <- df_q6_0.wide.nonen %>% 
  dplyr::select(-c(text)) %>%
  rename(text=translatedContent)
df_q6_0.wide <- rbind(df_q6_0.wide.nonen, df_q6_0.wide.en)
df_q6_0.wide <- df_q6_0.wide %>% dplyr::select(-lang)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/nlp_files'))
saveRDS(df_q6_0.wide, "df_q6_0_wide_2020_translated")


############## ~~~ CORPORATIONS ~~~ ###################################################################
############## Q2.4a: Opportunities #####################################################################

# Potenial connection to question Q3.2a in Cities.

### Preprocessing - 2020
df_corp_q2_4a <- df_corps.20 %>% filter(qstn == "C2.4a")
df_corp_q2_4a.wide <- dcast(df_corp_q2_4a, id + rown ~ coln, value.var="resp")
df_corp_q2_4a.wide <- df_corp_q2_4a.wide %>%
  rename("opp"="1", "value_chain"="2", "type"="3", "climate_driver"="4",
         "finicial_impact"="5", "specific_descr"="6", "horizon"="7", "likehood"="8",
         "magnitude_impact"="9", "provide_financial_figure"="10",
         "financial_figure"="11", "min_financial_figure"="12",
         "max_financial_figure"="13",  "financial_figure_descr"="14",
         "realize_cost"="15", "startegy_descr"="16", "comment"="17") %>%
  mutate(total_descr=paste(specific_descr, financial_figure_descr, startegy_descr, comment)) %>%
  filter(total_descr!="   ",)
# Save to RDS
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/nlp_files'))
saveRDS(df_corp_q2_4a.wide, "df_corp_q2_4a_wide_2020")


############## >>>> Cooperation between Cities and Corporation #####################################################################

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/nlp_files'))
# Load the data
df_corp_q2_4a.wide.20 <- readRDS("df_corp_q2_4a_wide_2020") %>%
  dplyr::select(total_descr) %>% rename(text=total_descr) %>%
  mutate(type="corporation", year="20")
# df_corp_q2_4a.wide.19 <- readRDS("df_corp_q2_4a_wide_2019") %>%
#   dplyr::select(total_descr) %>% rename(text=total_descr) %>%
#   mutate(type="corporation", year="19")
df_q6_0.wide.20 <- readRDS("df_q6_0_wide_2020_translated") %>%
  dplyr::select(text) %>%
  mutate(type="city", year="20") 
# df_q6_0.wide.19 <- readRDS("df_q6_0_wide_2019_translated") %>%
#   dplyr::select(text) %>%
#   mutate(type="city", year="19") 

# Combine in one dataset
# df_corp_city_text <- rbind(df_corp_q2_4a.wide.20, df_corp_q2_4a.wide.19,
#                            df_q6_0.wide.20, df_q6_0.wide.19)
# nrow(df_corp_city_text)
df_corp_city_text <- rbind(df_corp_q2_4a.wide.20,
                           df_q6_0.wide.20)
# df_corp_city_text <- df_corp_city_text %>% distinct(text, .keep_all = T)
nrow(df_corp_city_text)

# NLP preprocessing
# --- > !!! DIFFERENT CODE FOR KAGGLE NOTEBOOK, see "cdp_draft_r"
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = tolower(df_corp_city_text$text), verbose = 10)
df_corp_city_text$doc_id <- annotation$document$doc_id

# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT", "PROPN")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(doc_id, lemma) %>%
  group_by(doc_id) %>%
  summarise_at(vars(lemma), funs(paste(., collapse = ' ')))
df_corp_city_text <- df_corp_city_text %>% right_join(df_text_preprocessing)

# Load from RDS
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/nlp_files'))
saveRDS(df_corp_city_text, "df_corp_city_text")
df_corp_city_text <- readRDS("df_corp_city_text")

### Preprocessing with quanteda
corpus_corp_city <- corpus(df_corp_city_text$lemma,
                           docvars=df_corp_city_text %>% dplyr::select(-lemma))
dfmat_corp_city <- tokens(corpus_corp_city) %>%
  tokens_ngrams(n = c(1, 2)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 20)
ncol(dfmat_corp_city)
nrow(dfmat_corp_city)

# Remove documents without vocabulary
df_corp_city_text_temp <- df_corp_city_text[rowSums(dfmat_corp_city) != 0,]
dfmat_corp_city <- dfmat_corp_city[rowSums(dfmat_corp_city) != 0,]
nrow(dfmat_corp_city)
nrow(df_corp_city_text_temp)

# ~~ Model 2, K=20
K <- 20
stm_model.2 <- stm(documents = dfmat_corp_city,
                   prevalence =~ type,
                   data = dfmat_corp_city@docvars,
                   K = K, max.em.its = 75, init.type = "Spectral")
# Save for plotting
setwd("C:/Users/Artem/YandexDisk/CDP/data/for_plots")
saveRDS(stm_model.2, "stm_model.2")


# Extract topic labels
df_topiclabels <- data.frame(labelTopics(stm_model.2, n = 5)$frex)
df_topiclabels <- df_topiclabels %>%
  mutate(topic=as.character(1:nrow(df_topiclabels)),
         topic_label=paste0("Topic ", topic, ": ", paste(str_to_title(X1), X2, X3, X4, X5, sep = ", "))) %>%
  dplyr::select(topic_label, topic)

# Extract topics proportion
proportion <- as.data.frame(colSums(stm_model.2$theta/nrow(stm_model.2$theta)))
colnames(proportion) <- "topic_prop"
df_topiclabels <- cbind(df_topiclabels, proportion)
# Save for plotting
setwd("C:/Users/Artem/YandexDisk/CDP/data/for_plots")
saveRDS(df_topiclabels, "df_topiclabels.rds")

# Plot topic proportions
ggplot(df_topiclabels, aes(x = reorder(topic_label, topic_prop), y = topic_prop)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + theme_minimal() +
  scale_y_continuous(breaks = c(0, 0.3), limits = c(0, 0.3), expand = c(0, 0)) + #change breaks and limits as you need
  coord_flip() + 
  geom_text(aes(label = scales::percent(topic_prop, 2)), #Scale in percent
            hjust = -0.25, size = 4,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) + 
  theme(panel.border = element_blank())

# Calculating the Effects
prep.2 <- estimateEffect(1:K ~ type, stm_model.2,
                       uncertainty = "Global",
                       meta = dfmat_corp_city@docvars)
summary(prep.2, topics=c(1:K))

# Extract values for plotting
get_effects <- function(estimates,
                        # estimates object
                        variable,
                        # variable for estimates
                        type,
                        # continuous or pointestimate
                        ci = 0.95,
                        # confidence interval
                        moderator = NULL,
                        # moderator for interaction
                        modval = NULL,
                        # cov values for difference
                        cov_val1 = NULL,
                        cov_val2 = NULL) {
  data <- plot.estimateEffect(
    x = estimates,
    covariate = variable,
    method = type,
    ci.level = ci,
    moderator = moderator,
    moderator.value = modval,
    cov.value1 = cov_val1,
    cov.value2 = cov_val2,
    omit.plot = TRUE
  )
  
  
  # catching inconsistent stm naming conventions
  if (!'cis' %in% names(data)) {
    data$cis <- data$ci
  }
  
  if ('x' %in% names(data)) {
    data$uvals <- data$x
  }
  names(data$cis) <- data$topics
  names(data$means) <- data$topics
  
  tidy_stm <- data$topics %>% purrr::map(function(top) {
    top <- as.character(top)
    
    if (type == 'difference') {
      props <- tibble(
        difference = data$means[[top]],
        topic = top,
        lower = data$cis[[top]][[1]],
        upper = data$cis[[top]][[2]]
      )
    }
    
    else {
      cis <- t(data$cis[[top]]) %>% as_tibble() %>%
        purrr::set_names(c('lower', 'upper'))
      
      props <-
        tibble(
          value = data$uvals,
          proportion = data$means[[top]],
          topic = top
        ) %>%
        bind_cols(cis)
    }
  })
  
  tidy_stm <- tidy_stm %>% bind_rows()
  tidy_stm$topic <- as.factor(tidy_stm$topic)
  if (type == 'pointestimate') {
    tidy_stm$value <- as.factor(tidy_stm$value)
  }
  if (!is.null(moderator)) {
    tidy_stm$moderator <- modval
  }
  return(tidy_stm)
}


df_effects <- get_effects(prep.2, variable = "type", type = "difference",
                          cov_val1  = "corporation", cov_val2 = "city")
df_effects$topic <- as.character(sort(as.numeric(df_effects$topic), decreasing = T))
df_effects <- df_effects %>% left_join(df_topiclabels)
# Save for plotting
setwd("C:/Users/Artem/YandexDisk/CDP/data/for_plots")
saveRDS(df_effects, "df_effects")

# Plot
ggplot(df_effects, aes(x = reorder(topic_label, difference),
                    y = difference)) + 
  
  geom_point(data=df_effects, size=2) +
  ylim(-0.3, 0.3) + 
  geom_errorbar(aes(ymin = lower, ymax = upper),  width=0.25) + 
  
  labs(x = "",
       y = "Corporation < ................... > City") + 
  geom_hline(aes(yintercept=0), 
             linetype="dashed", color = "gray40") +
  coord_flip() + theme_minimal() +
  theme(axis.title=element_text(size=16),
        legend.text=element_text(size=16),
        # legend.position = c(0.9, 0.1),
        legend.title=element_text(size=16),
        text = element_text(size=16))

labelTopics(stm_model.2, topics = c(7, 11))
labelTopics(stm_model.2, topics = c(14, 20))
labelTopics(stm_model.2, topics = c(18, 16, 3))

stm::cloud(stm_model.2, topic = 18, scale = c(2,.5))
stm::cloud(stm_model.2, topic = 16, scale = c(2,.5))
stm::cloud(stm_model.2, topic = 3, scale = c(2,.5))






