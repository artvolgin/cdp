
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
# Other
library(arules)
library(arulesViz)
library(reshape2)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Cities/Cities Responses'))

# --- Load the 2020 data
df_cities.20 <- read.csv('2020_Full_Cities_Dataset.csv') %>% 
  select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
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
  select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
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


#############################################################################################################
############## 1. Network of Cities
#############################################################################################################


### --- Q3.3, 2020

# Load the question and specific column
df_q3_3 <- df_cities.20 %>% filter(qstn == "3.3")
length(unique(df_q3_3$org))
# Select one column
df_q3_3.init <- df_q3_3 %>%
  filter(colname == "Select the initiatives related to this adaptation goal that your city has committed to")
length(unique(df_q3_3.init$org))

# Remove non-substantive answers
remove_resp <- c("", "Individual City Commitment", "Declaring Climate Emergency",
                 "Other", "This target does not contribute towards an initiative commitment")
df_q3_3.init <- df_q3_3.init %>% filter(!(resp %in% remove_resp))
df_q3_3.init <- df_q3_3.init %>% distinct(id,resp, .keep_all= TRUE)
t <- as.data.frame(table(df_q3_3.init$resp))

# Mean number of aggrements per city
nrow(df_q3_3.init) / length(unique(df_q3_3.init$id))

# Transform to network
g <- graph_from_edgelist(as.matrix(df_q3_3.init %>% dplyr::select(org, resp)))
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "square", "circle")
E(g)$color <- "lightgray"
plot(g,
     vertex.label.cex = 0.5,
     vertex.label=NA,
     vertex.label.color = "black",
     vertex.size=3,
     edge.arrow.size=0.1)

# Transform to one-mode network
bipartite_matrix  <- as_incidence_matrix(g)
city_matrix <- bipartite_matrix %*% t(bipartite_matrix)
diag(city_matrix) <- 0
g.city <- graph_from_adjacency_matrix(city_matrix)
plot(g.city,
     vertex.label.cex = 0.5,
     vertex.label=NA,
     vertex.label.color = "black",
     vertex.size=3,
     edge.arrow.size=0.1)


### --- Q1.1a, 2019

df_q1_1a <- df_cities.19 %>%
  filter(qstn=="1.1a", colname=="Name of commitment and attach document")
# Remove non-substantive answers
remove_resp <- c("", "Individual city commitment", "Other")
df_q1_1a <- df_q1_1a %>% filter(!(resp %in% remove_resp))
df_q1_1a <- df_q1_1a %>% distinct(id,resp, .keep_all= TRUE)
sum(df_q1_1a$resp == "Global Covenant of Mayors for Climate & Energy") / length(unique(df_q1_1a$id))
nrow(df_q1_1a) / length(unique(df_q1_1a$id))
t <- as.data.frame(table(df_q1_1a$resp))

# Transform to network
g <- graph_from_edgelist(as.matrix(df_q1_1a %>% dplyr::select(org, resp)))
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "square", "circle")
E(g)$color <- "lightgray"
plot(g,
     vertex.label.cex = 0.5,
     vertex.label=NA,
     vertex.label.color = "black",
     vertex.size=3,
     edge.arrow.size=0.1)

#  https://rpubs.com/pjmurphy/317838

# Transform to one-mode network
bipartite_matrix  <- as_incidence_matrix(g)
city_matrix <- bipartite_matrix %*% t(bipartite_matrix)
diag(city_matrix) <- 0
g.city <- graph_from_adjacency_matrix(city_matrix)
g.city <- as.undirected(g.city)
plot(g.city,
     vertex.label.cex = 0.5,
     vertex.label=NA,
     vertex.label.color = "black",
     vertex.size=3)



#############################################################################################################
############## Q2.1 Hazards
#############################################################################################################


#############################################################################################################
############## Q2.2: NLP 
#############################################################################################################



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

### NLP preprocessing
corpus_q2_2 <- corpus(df_q2_2.wide$text,
       docvars=df_q2_2.wide %>% dplyr::select(-c(text)))

tokens_q2_2 <- tokens_replace(tokens(corpus_q2_2),
                              pattern = lexicon::hash_lemmas$token,
                              replacement = lexicon::hash_lemmas$lemma)
              # tokens_ngrams(n = 2) 
dfmat_q2_2 <- dfm(tokens_q2_2) %>% 
        dfm(remove = stopwords("en"), remove_number = TRUE, stem = F) %>% 
        dfm_trim(min_termfreq = 50)
lasso <- glmnet(x = dfmat_q2_2,
                y = as.integer(dfmat_q2_2@docvars$support_challenges == "Supports"),
                alpha = 1,
                family = "binomial")
index_best <- which(lasso$lambda == min(lasso$lambda))
beta <- lasso$beta[, index_best]

head(sort(beta, decreasing = T), 20)
head(sort(beta, decreasing = F), 20)



# generate 1500 numbers without replacement
set.seed(300)
id_train <- sample(1:length(corpus_q2_2), 1500, replace = FALSE)

# create docvar with ID
corpus_q2_2$id_numeric <- 1:ndoc(corpus_q2_2)


corpus_q2_2 <- corpus(tokens_replace(tokens(corpus_q2_2),
               pattern = lexicon::hash_lemmas$token,
               replacement = lexicon::hash_lemmas$lemma))

toks_inaug <- tokens(data_corpus_inaugural, remove_punct = TRUE,
                    remove_number = TRUE)


# get training set
dfmat_training <- corpus_subset(corpus_q2_2, id_numeric %in% id_train) %>%
  dfm(remove = stopwords("en"), remove_number = TRUE, stem = F) %>% 
  dfm_trim(min_termfreq = 10)

# get test set (documents not in id_train)
dfmat_test <- corpus_subset(corpus_q2_2, !id_numeric %in% id_train) %>%
  dfm(remove = stopwords("en"), remove_number = TRUE, stem = TRUE) %>%
  dfm_trim(min_termfreq = 10)

lasso <- cv.glmnet(x = dfmat_training,
                   y = as.integer(dfmat_training@docvars$support_challenges == "Supports"),
                   alpha = 1,
                   nfold = 5,
                   family = "binomial")

index_best <- which(lasso$lambda == lasso$lambda.min)
beta <- lasso$glmnet.fit$beta[, index_best]

head(sort(beta, decreasing = T), 20)



#############################################################################################################
############## Q3: Association Rules
#############################################################################################################

### --- Preprocessing
df_q3_0 <- df_cities.20 %>% filter(qstn == "3.0")
df_q3_0.benifit <- df_q3_0 %>% filter(colname %in% c("Co-benefit area"), resp!="")
df_q3_0.hazards <- df_q3_0 %>% filter(colname %in% c("Climate hazards"), resp!="")
df_q3_0.actions <- df_q3_0 %>% filter(colname %in% c("Action"), resp!="")


### --- 1. Co-benifits

# To id-row format
df_q3_0.benifit <- df_q3_0.benifit %>%
  group_by(id, rown) %>%
  summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
  ungroup() %>%  # Check 
  dplyr::select(resp) %>%
  rename(items=resp)

### Association Rules
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
export(df_q3_0.benifit, file = "df-q3-0-benifit.csv", "csv")
tr <- read.transactions("df-q3-0-benifit.csv", format = 'basket', sep=';')
summary(tr)
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5, maxlen=3))
# subRules <- association.rules[quality(association.rules)$confidence>0.9]
top10subRules <- head(association.rules, n = 10, by = "confidence")
top10subRules <- head(association.rules, n = 10, by = "count")
plot(top10subRules, method = "graph",  engine = "htmlwidget")


### --- 2. Hazards

# To id-row format
df_q3_0.hazards <- df_q3_0.hazards %>%
  group_by(id) %>%
  summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
  ungroup() %>%  # Check 
  dplyr::select(resp) %>%
  rename(items=resp)

### Association Rules
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
export(df_q3_0.hazards, file = "df-q3-0-hazards.csv", "csv")
tr <- read.transactions("df-q3-0-hazards.csv", format = 'basket', sep=';')
summary(tr)
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.9, maxlen=2))
# subRules <- association.rules[quality(association.rules)$confidence>0.9]
#top10subRules <- head(association.rules, n = 10, by = "confidence")
top10subRules <- head(association.rules, n = 10, by = "count")
plot(top10subRules, method = "graph",  engine = "htmlwidget")


### --- 3. Actions 

# To id-row format
df_q3_0.actions <- df_q3_0.actions %>%
  group_by(id) %>%
  summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
  ungroup() %>%  # Check 
  dplyr::select(resp) %>%
  rename(items=resp)

### Association Rules
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
export(df_q3_0.benifit, file = "df-q3-0-benifit.csv", "csv")
tr <- read.transactions("df-q3-0-benifit.csv", format = 'basket', sep=';')
summary(tr)
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.9, maxlen=10))
# subRules <- association.rules[quality(association.rules)$confidence>0.9]
top10subRules <- head(association.rules, n = 10, by = "confidence")
top10subRules <- head(association.rules, n = 10, by = "count")
plot(top10subRules, method = "graph",  engine = "htmlwidget")


################################## TODO

# 1. Hazards --> Actions --> Co-benefits. What are the benefits for each Hazard?


#############################################################################################################
############## Wikipedia Data
#############################################################################################################

library(WikidataR)

aarons <- find_item("London")
aarons









