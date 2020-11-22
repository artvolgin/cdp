
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

############## Q2.0b, Q3.0, Q5.5a : NLP, 2020 #########################################################################

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

### 2. Correspondense analysis, Documents position
tmod_ca <- textmodel_ca(dfm_q2_0b)
textplot_scale1d(tmod_ca)
dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
head(dat_ca)
plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))

# 3. LDA, Topics
tmod_lda <- textmodel_lda(dfm_q2_0b, k = 10)
terms(tmod_lda, 10)


### --------- Q3.0 

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

# TODO: FIX LATER
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
         !(upos %in% c("DET", "PUNCT")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(id, lemma) %>%
  group_by(id) %>%
  summarise_at(vars(lemma), funs(paste(., collapse = ' ')))

# Create Corpus and Document term matrix
corpus_q3_0 <- corpus(df_text_preprocessing$lemma,
                       docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q3_0 <- tokens(corpus_q3_0) %>%
  tokens_ngrams(n = c(1)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)
ncol(dfm_q3_0)

### 1. STM 
stm_model <- stm(documents = dfm_q3_0,
                 K = 5, max.em.its = 75, init.type = "Spectral")
labelTopics(stm_model)
plot(stm_model, type = "summary")

### 2. Correspondense analysis, Documents position
tmod_ca <- textmodel_ca(dfm_q2_0b)
textplot_scale1d(tmod_ca)
dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
head(dat_ca)
plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))



### --------- Q5.5a
# TODO

df_q5_5a <- df_cities.20 %>% filter(qstn == "5.5a")
df_q5_5a.link <- df_q5_5a %>%
  filter(colname == "Web link",
         !(resp %in% c("Question not applicable", ""))) %>%
  dplyr::select(id, resp)
# Extract the links
temp_vec <- sapply(df_q5_5a.link$resp, function(x) str_extract_all(x, "(?<=^|\\s)http[^\\s]+"))
temp_vec <- as.data.frame(sapply(temp_vec, function(x) paste(x, collapse = ' ')))
df_q5_5a.link$resp <- temp_vec[,1]
df_q5_5a.link <- df_q5_5a.link %>% filter(resp!="")
df_q5_5a.link <- df_q5_5a.link %>% separate(resp, paste0("link", as.character(1:9)),
                                          sep=" ")
df_q5_5a.link <- df_q5_5a.link[,colSums(is.na(df_q5_5a.link)) != nrow(df_q5_5a.link)]



############## Q2.0b, Q3.0, Q5.5a : NLP, 2019 ################################################################

# TODO:
# 1. Look for .pdf files in the page, TOO COMLICATED
# 2. Same questions in 2019 and 2018

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
setwd("C:/Users/Artem/YandexDisk/CDP/data/ParsedReports/2019/q2_0b")
oldw <- getOption("warn")
options(warn = -1)
for (i in (1:nrow(df_q2_0b.link.long))) {
  tryCatch(download.file(df_q2_0b.link.long$link[i],
                         destfile = paste0(df_q2_0b.link.long$id_link_num[i], ".pdf"),
                         mode = "wb", quiet = T), 
           error = function(e) print(paste(as.character(i), ' error ~~~~~')))
  print(i)
  Sys.sleep(0.5)
}
options(warn = oldw)

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

### TODO: Something with Named Entity Recognisition

# Most popular organization?
t.1 <- annotation$entity %>% filter(entity_type=="ORG") %>%
  dplyr::select(entity) %>% table() %>% as.data.frame()

# Federal VS. Local level?
t.2 <- annotation$entity %>% filter(entity_type=="GPE") %>%
  dplyr::select(entity) %>% table() %>% as.data.frame()




# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(doc_id, lemma) %>%
  group_by(id) %>%
  # group_by(doc_id) %>% # PAGE AS THE UNIT OF OBSERVATION <<<<<<<<<<<<<<<<<<<<<<<<
  summarise_at(vars(lemma), funs(paste(., collapse = ' ')))

# Remove pages with low number of characters
df_text_preprocessing$len <- unlist(lapply(df_text_preprocessing$lemma, nchar))
df_text_preprocessing <- df_text_preprocessing %>% filter(len>200)

# Create Corpus and Document term matrix
corpus_q2_0b <- corpus(df_text_preprocessing$lemma,
                      docvars=df_text_preprocessing %>% dplyr::select(-c(lemma)))
dfm_q2_0b <- tokens(corpus_q2_0b) %>%
  tokens_ngrams(n = c(1, 2)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)
ncol(dfm_q2_0b)

### 1. STM 
stm_model <- stm(documents = dfm_q2_0b,
                 K = 10, max.em.its = 75, init.type = "Spectral")
labelTopics(stm_model)
plot(stm_model, type = "summary", n = 5, xlim = c(0,1))

mod.out.corr <- topicCorr(stm_model, cutoff = 0.0000000000001)
plot(mod.out.corr)

### 2. Correspondense analysis, Documents position
tmod_ca <- textmodel_ca(dfm_q2_0b)
textplot_scale1d(tmod_ca)
dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
head(dat_ca)
plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))

# 3. LDA, Topics
tmod_lda <- textmodel_lda(dfm_q2_0b, k = 10)
terms(tmod_lda, 10)




############## Q6.0 : NLP  ####################################################################

# TODO: Looks promising!
df_q6_0 <- df_cities.20 %>% filter(qstn == "6.0")

df_q6_0.opport <- df_q6_0 %>%
  filter(colname == "Opportunity")
sort(table(df_q6_0.opport$resp))
df_q6_0.descr <- df_q6_0 %>%
  filter(colname == "Describe how the city is maximizing this opportunity", resp != "")


############## Q6.5 : NLP ###################################################################################

### Additional Preprocessing for Currency
df_currency <- df_cities.20 %>%
  filter(qstn == "0.4", resp!="") %>%
  dplyr::select(id, resp) %>% 
  rename(currency=resp) %>%
  mutate(currency=substr(currency, 1, 3)) 
currency_rate_usd <- getQuote(paste0(df_currency$currency, rep("USD", nrow(df_currency)), "=X"))
currency_rate_usd <- currency_rate_usd %>%
  dplyr::select(Last) %>% mutate(currency=substr(rownames(currency_rate_usd), 1, 3)) %>%
  rename(curr_rate=Last)
df_currency <- df_currency %>% left_join(currency_rate_usd) %>% select(-c(currency))

### Main Preprocessing
df_q6_5 <- df_cities.20 %>% filter(qstn == "6.5")
df_q6_5.wide <- dcast(df_q6_5, id + rown ~ coln, value.var="resp")
df_q6_5.wide <- df_q6_5.wide %>%
  rename("area"="1", "title"="2", "stage"="3", "status_financing"="4", "fin_model"="5",
         "fin_model_descr"="6", "descr"="7", "cost"="8", "cost_needed"="9") %>%
  filter(!(area %in% c("No relevant projects", "")), descr!="", cost!="") %>%
  mutate(cost=as.numeric(cost)) %>%
  filter(cost>0)
df_q6_5.wide <- df_q6_5.wide %>% left_join(df_currency) %>% mutate(cost_usd=cost*curr_rate)
hist(log(df_q6_5.wide$cost_usd), breaks=50)

### NLP Preprocessing

# Translation
# df_q6_5.wide <- df_q6_5.wide %>% left_join(city_lang)
# df_q6_5.wide.en <- df_q6_5.wide %>% filter(lang=="en")
# df_q6_5.wide.nonen <- df_q6_5.wide %>% filter(lang!="en")
# df_list <- list()
# unique_langs <- unique(df_q6_5.wide.nonen$lang)
# for (i in (1:length(unique_langs))){
#   
#   df_temp <- df_q6_5.wide.nonen %>% filter(lang==unique_langs[i])
#   df_temp <- translate(dataset = df_temp,
#                        content.field = 'text',
#                        google.api.key = 'AIzaSyCyADH7ABEw5_KB8e_pG2e9Qc6Aa720Jmg',
#                        source.lang = unique_langs[i],
#                        target.lang = 'en')
#   df_list[[i]] <- df_temp
#   print(unique_langs[i])
#   
# }
# df_q6_5.wide.nonen <- do.call(rbind, df_list)
# df_q6_5.wide.nonen <- df_q6_5.wide.nonen %>% 
#   dplyr::select(-c(text)) %>%
#   rename(text=translatedContent)
# df_q6_5.wide <- rbind(df_q6_5.wide.nonen, df_q6_5.wide.en)


### Preprocessing with cleanNLP
# --- > !!! DIFFERENT CODE FOR KAGGLE NOTEBOOK, see "cdp_draft_r"
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = tolower(df_q6_5.wide$descr))
df_q6_5.wide$doc_id <- annotation$document$doc_id
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
df_q6_5.wide <- df_q6_5.wide %>% right_join(df_text_preprocessing)
df_q6_5.wide <- df_q6_5.wide %>% filter(!is.na(cost_usd)) %>%
  dplyr::mutate(cost_usd_cat=as.integer(cost_usd >= mean(cost_usd)))

### Preprocessing with quanteda
corpus_q6_5 <- corpus(df_q6_5.wide$lemma,
                      docvars=df_q6_5.wide %>% dplyr::select(-c(descr, lemma)))
dfmat_q6_5 <- tokens(corpus_q6_5) %>%
  tokens_ngrams(n = c(1,2)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)
ncol(dfmat_q6_5)

# Binomial GLM
lasso <- glmnet(x = dfmat_q6_5,
                y = dfmat_q6_5@docvars$cost_usd_cat,
                alpha = 1,
                family = "binomial")
index_best <- which(lasso$lambda == min(lasso$lambda))
beta <- lasso$beta[, index_best]
head(sort(beta, decreasing = T), 20)
head(sort(beta, decreasing = F), 20)






############## ~~~ CORPORATIONS ~~~ ###################################################################

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Corporations/Corporations Responses/Climate Change'))

# --- Load the 2020 data
df_corps.20 <- read.csv('2020_Full_Climate_Change_Dataset.csv') %>% 
  select('id' = account_number, 'org' = organization, 
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


############## Q2.4a: Opportunities #####################################################################

# TODO: Comperehnsive question. A lot can be done here. 
# Potenial connection to question Q3.2a in Cities.


### Preprocessing
df_corp_q2_4a <- df_corps.20 %>% filter(qstn == "C2.4a")
df_corp_q2_4a.wide <- dcast(df_corp_q2_4a, id + rown ~ coln, value.var="resp")
df_corp_q2_4a.wide <- df_corp_q2_4a.wide %>%
  rename("opp"="1", "value_chain"="2", "type"="3", "climate_driver"="4",
         "finicial_impact"="5", "specific_descr"="6", "horizon"="7", "likehood"="8",
         "magnitude_impact"="9", "provide_financial_figure"="10",
         "financial_figure"="11", "min_financial_figure"="12",
         "max_financial_figure"="13",  "financial_figure_descr"="14",
         "realize_cost"="15", "startegy_descr"="16", "comment"="17") %>%
  mutate(total_descr=paste(specific_descr, financial_figure_descr, startegy_descr, comment),
         magnitude_impact_bin=dplyr::recode(magnitude_impact,
                                            "High"='High', "Medium-high"="High", "Medium"="High",
                                            "Medium-low"="Low", "Low"="Low"),
         realize_cost_log=log(as.numeric(realize_cost))) %>%
  filter(total_descr!="   ",
         # !(magnitude_impact_bin %in% c("", "Unknown")),
         !is.na(realize_cost_log),
         realize_cost_log>0)

# TODO: Investigate the part about financial figure
hist(df_corp_q2_4a.wide$realize_cost_log, breaks = 20)


### Preprocessing with cleanNLP
# --- > !!! DIFFERENT CODE FOR KAGGLE NOTEBOOK, see "cdp_draft_r"
# Initilize spacy model
cnlp_init_spacy("en_core_web_sm")

# Annotate the text
annotation <- cnlp_annotate(input = tolower(df_corp_q2_4a.wide$total_descr))
df_corp_q2_4a.wide$doc_id <- annotation$document$doc_id

# Remove stopwords and non-alphabetical tokens
stopwords_vec <- stopwords::stopwords(language = "en",source = "smart")
stopwords_vec <- c(stopwords_vec, "e.g.", "la")
df_text_preprocessing <- annotation$token %>%
  filter(!(lemma %in% stopwords_vec),
         !(upos %in% c("DET", "PUNCT")),
         (grepl("^[A-Za-z]+$", lemma, perl = T))) %>%
  dplyr::select(doc_id, lemma) %>%
  group_by(doc_id) %>%
  summarise_at(vars(lemma), funs(paste(., collapse = ' ')))
df_corp_q2_4a.wide <- df_corp_q2_4a.wide %>% right_join(df_text_preprocessing)

### Preprocessing with quanteda
corpus_q2_4a <- corpus(df_corp_q2_4a.wide$lemma,
                      docvars=df_corp_q2_4a.wide %>% dplyr::select(-c(total_descr, lemma)))
dfmat_q2_4a <- tokens(corpus_q2_4a) %>%
  tokens_ngrams(n = c(1)) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 10)
ncol(dfmat_q2_4a)

# Remove documents without vocabulary
dfmat_q2_4a <- dfmat_q2_4a[rowSums(dfmat_q2_4a) != 0,]

### Topic Modelling, Wordfish and Correspondence analysis

# Group by ID and merge all answers for the corporation
# df_corp_q2_4a.wide.id <- df_corp_q2_4a.wide %>%
#   group_by(id) %>%
#   summarise_at(vars(lemma), funs(paste(., collapse = ' '))) %>%
#   ungroup()
# 
# # Preprocessing with quanteda
# corpus_q2_4a.id <- corpus(df_corp_q2_4a.wide.id$lemma)
# dfmat_q2_4a.id <- tokens(corpus_q2_4a.id) %>%
#   tokens_ngrams(n = c(1)) %>%
#   dfm() %>%
#   dfm_trim(min_termfreq = 1)
#   # dfm_tfidf()
# ncol(dfmat_q2_4a.id)
# sum(rowSums(dfmat_q2_4a.id) == 0)


### 0. Binomial GLM
lasso <- glmnet(x = dfmat_q2_4a,
                y = dfmat_q2_4a@docvars$magnitude_impact_bin,
                alpha = 1,
                family = "binomial")
index_best <- which(lasso$lambda == min(lasso$lambda))
beta <- lasso$beta[, index_best]
head(sort(beta, decreasing = T), 20)
head(sort(beta, decreasing = F), 20)

# 1. Wordfish, Words position
tmod_wf <- textmodel_wordfish(dfmat_q2_4a.id, dir = c(6, 5))
textplot_scale1d(tmod_wf)
textplot_scale1d(tmod_wf, margin = "features")

# 2. Correspondense analysis, Documents position
tmod_ca <- textmodel_ca(dfmat_q2_4a.id)
textplot_scale1d(tmod_ca)

# 3. LDA, Topics
tmod_lda <- textmodel_lda(dfmat_q2_4a.id, k = 5)
terms(tmod_lda, 10)

# 4. STM 
# TODO: How to find optimal number number of topics? "K=0" or other methods?
# stm_model <- stm(documents = dfmat_q2_4a,
#                  K = 0, max.em.its = 75, init.type = "Spectral")
# stm_model.search <- searchK(documents = dfmat_q2_4a, K = c(5, 10))

# STM Model 1.
stm_model.1 <- stm(documents = dfmat_q2_4a,
                 K = 10, max.em.its = 75, init.type = "Spectral")

# STM Model 2. With Realize cost
stm_model.2 <- stm(documents = dfmat_q2_4a,
                 prevalence =~ realize_cost_log,
                 data = dfmat_q2_4a@docvars,
                 K = 10, max.em.its = 75, init.type = "Spectral")
prep <- estimateEffect(1:10 ~ realize_cost_log, stm_model.2,
                       uncertainty = "Global",
                       meta = dfmat_q2_4a@docvars)
summary(prep, topics=c(1:10))

# STM Model 3. With Opportunity type and K=20
stm_model.3 <- stm(documents = dfmat_q2_4a,
                   prevalence =~ type,
                   data = dfmat_q2_4a@docvars,
                   K = 20, max.em.its = 75, init.type = "Spectral")

prep <- estimateEffect(1:20 ~ type, stm_model.3,
                       uncertainty = "Global",
                       meta = dfmat_q2_4a@docvars)
summary(prep, topics=c(1:20))

plot(prep, covariate = "type", topics = c(1:20),
     model = stm_model.3, method = "difference",
     cov.value1 = "Markets", cov.value2 = "Resilience",
     xlab = "Resilience < ....... > Markets",
     main = "Topics by the Opportunity Type",
     xlim = c(-.3, .3),
     labeltype = "custom")


labelTopics(stm_model.3)
cloud(stm_model.2, topic = 8, scale = c(5,1))
plot(stm_model.2, type = "perspectives", topics = c(1, 7))
mod.out.corr <- topicCorr(stm_model.2)
plot(mod.out.corr)




################################## TODO ##############################################

# 1. Hazards --> Actions --> Co-benefits. What are the benefits for each Hazard?
# 2. Add Mitigation Actions (Question 5.4)
# 3. Ivestigate questions with large number of rows   
# 4. Connect Add C2.4
# t <- as.data.frame(table(df_cities.20$qstn))







