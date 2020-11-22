
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
temp_vec <- df_cities.20$org
Encoding(temp_vec) <- "UTF-8"
df_cities.20$org <- temp_vec
# Recode 'Other' category
df_cities.20$resp[grepl("^Other, please specify", as.character(df_cities.20$resp))] <- "Other"
# Calculate question with longest answer
# df_cities.20$len <- unlist(lapply(df_cities.20$resp, nchar))
# t <- df_cities.20 %>% group_by(qstname) %>% summarise(mean_len=mean(len, na.rm=T))

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


#############################################################################################################
############## Corporations, Q0.4, 2020: Network of Countries based on the common Corporations
#############################################################################################################

### Preprocessing
df_corp_q0_4 <- df_corps.20 %>%
  filter(qstn == "C0.3", resp!="") %>%
  dplyr::select(id, resp)

# TO LONLG
df_corp_q0_4 <- df_corp_q0_4 %>% separate(resp, paste0("country", as.character(1:200)), sep="; ")
df_corp_q0_4 <- df_corp_q0_4[,colSums(is.na(df_corp_q0_4)) != nrow(df_corp_q0_4)]
# Remove corporations that are located only in one country
df_corp_q0_4 <- df_corp_q0_4 %>% filter(rowSums(is.na(df_corp_q0_4)) != (ncol(df_corp_q0_4) - 2))

# Transform to long-format: corporation-country as observation
df_corp_q0_4 <- melt(df_corp_q0_4, id.vars=c("id")) %>%
  filter(!is.na(value)) %>% rename(cntry_number=variable, cntry_name=value)

# Remove countries with low-frequency
countries_freq <- as.data.frame(table(df_corp_q0_4$cntry_name)) %>% filter(Freq >= 10)
df_corp_q0_4 <- df_corp_q0_4 %>%
  filter(cntry_name %in% unique(as.character(countries_freq$Var1)))

# Transform to network
g <- graph_from_edgelist(as.matrix(df_corp_q0_4 %>% dplyr::select(id, cntry_name)))
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

country_matrix <- t(bipartite_matrix) %*% bipartite_matrix
diag(country_matrix) <- 0
country_matrix[country_matrix < 100] <- 0
g.country <- graph_from_adjacency_matrix(country_matrix, weighted = T)
g.country <- as.undirected(g.country)

plot(g.country,
     vertex.label.cex = 0.5,
     # vertex.label=T,
     vertex.label.color = "black",
     vertex.size=3,
     edge.width=(E(g.country)$weight)*0.01,
     edge.color=adjustcolor("gray", alpha=0.5),
     layout=layout_with_kk)

### Community detection
louvain.result <- cluster_louvain(g.country, weights = E(g.country)$weight)
df_louvain.result <- data.frame(cntry_name=louvain.result$names,
                                community=louvain.result$membership)

### PLOT ON THE MAP

# --- Set the Nodes
nodes.country <- data.frame("cntry_name"=V(g.country)$name)
register_google(key = "AIzaSyBXXfPn9HfpPLzLQUyYjMVoUwvN7MRgNnk")
nodes.country.coords <- geocode(nodes.country$cntry_name)
nodes.country <- nodes.country %>% cbind(nodes.country.coords)
nodes.country <- nodes.country %>% filter(!is.na(lat))

# --- Set the Edges
# Create edgelist
edgelist.country <- as.data.frame(as_edgelist(g.country, names = TRUE))
edgelist.country$weight <- E(g.country)$weight


# --- Network Creation
net.country <-network(edgelist.country,
                 matrix.type='edgelist',
                 directed=FALSE,  # this will be an undirected network
                 ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                 names.eval='weight'  # names for the edge weights
)

# Add coordinates to the nodes
net.country%v%'lon' <- sapply(network.vertex.names(net.country),function(name){
  nodes.country[nodes.country$cntry_name==name,]$lon
})

net.country%v%'lat' <- sapply(network.vertex.names(net.country),function(name){
  nodes.country[nodes.country$cntry_name==name,]$lat
})



# plot the map for the background
maps::map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)

# plot the network using the geo coordinates
plot.network(net.country,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(net.country%v%'lon', net.country%v%'lat'),  
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             edge.col='#AA555555',
             # specifiy an edge width scaled as fraction of total co-occurence
             # edge.lwd=net.country%e%'weight'/1000,
             edge.lwd=((net.country%e%'weight')^2)/100000,
             # set the vertex size
             vertex.cex=0.3,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='white',
             # please don't jitter the points around
             jitter=FALSE,
             xlim = c(-200,200),
             ylim= c(-200,200))

### ANOTHER EXAMPLE
# library(maps)
# library(geosphere)
# library(dplyr)
# library(nycflights13)
# 
# 
# usairports <- filter(airports, lat < 48.5)
# usairports <- filter(usairports, lon > -130)
# usairports <- filter(usairports, faa!="JFK")
# jfk <- filter(airports, faa=="JFK")
# 
# 
# map("world", fill=T, col="grey8", bg="grey15")
# 
# 
# for (i in (1:dim(usairports)[1])) { 
#   
#   inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
#   
#   lines(inter, lwd=0.1, col="turquoise2")    
# }
# 
# points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")
# 


#############################################################################################################
############## Cities, Q3.3, 2020: Network of Cities based on the common Organizations
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
############## Cities, Q3, 2020: Association Rules
#############################################################################################################

### --- 0. Hazards, Actions and Benifits together

### --- 0.1 Preprocessing BY ID-ROWN
df_q3_0 <- df_cities.20 %>% filter(qstn == "3.0",
                                   colname %in% c("Co-benefit area", "Climate hazards", "Action"))

df_q3_0.hazards <- df_q3_0 %>% filter(colname %in% c("Climate hazards"), resp!="") %>%
  dplyr::select(id, rown, resp) %>% rename(hazard=resp) %>%
  mutate(hazard=unlist(sapply(strsplit(hazard, "\\ > "), "[", 1)), # Merge to large groups
         hazard=dplyr::recode(hazard, "Biological hazards "="Biological hazards")) %>%
  filter(hazard!="Question not applicable")

df_q3_0.actions <- df_q3_0 %>% filter(colname %in% c("Action"), resp!="")
df_q3_0.benifits <- df_q3_0 %>% filter(colname %in% c("Co-benefit area"), resp!="")

df_q3_0.actions <- df_q3_0.actions %>%
  group_by(id, rown) %>%
  summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
  ungroup() %>% rename(actions=resp)

df_q3_0.benifits <- df_q3_0.benifits %>%
  group_by(id, rown) %>%
  summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
  ungroup() %>% rename(benifits=resp)

### Version 1: Hazards, Actions, Benifits
df_q3_0.wide <- df_q3_0.hazards %>%
  full_join(df_q3_0.actions, by = c("id", "rown")) %>%
  full_join(df_q3_0.benifits, by = c("id", "rown"))
df_q3_0.wide <- df_q3_0.wide %>% filter(rowSums(is.na(df_q3_0.wide)) == 0)
df_q3_0.wide <- df_q3_0.wide %>% mutate(
  hazard_actions_benifits=paste(hazard, actions, benifits, sep = ";"))
df_q3_0.wide.rules <- df_q3_0.wide %>% dplyr::select(hazard_actions_benifits) %>%
  rename(items=hazard_actions_benifits)

### Version 2: Actions, Benifits
# df_q3_0.wide <- df_q3_0.actions %>%
#   full_join(df_q3_0.benifits, by = c("id", "rown"))
# df_q3_0.wide <- df_q3_0.wide %>% filter(rowSums(is.na(df_q3_0.wide)) == 0)
# df_q3_0.wide <- df_q3_0.wide %>% mutate(
#   actions_benifits=paste(actions, benifits, sep = ";"))
# df_q3_0.wide.rules <- df_q3_0.wide %>% dplyr::select(actions_benifits) %>%
#  rename(items=actions_benifits)
# 
# ### Version 3: Hazards, Actions
# df_q3_0.wide <- df_q3_0.hazards %>%
#   full_join(df_q3_0.actions, by = c("id", "rown"))
# df_q3_0.wide <- df_q3_0.wide %>% filter(rowSums(is.na(df_q3_0.wide)) == 0)
# df_q3_0.wide <- df_q3_0.wide %>% mutate(
#   hazard_actions=paste(hazard, actions, sep = ";"))
# df_q3_0.wide.rules <- df_q3_0.wide %>% dplyr::select(hazard_actions) %>%
#   rename(items=hazard_actions)
# 
# ### Version 4: Hazards, Benifits
# df_q3_0.wide <- df_q3_0.hazards %>%
#   full_join(df_q3_0.benifits, by = c("id", "rown"))
# df_q3_0.wide <- df_q3_0.wide %>% filter(rowSums(is.na(df_q3_0.wide)) == 0)
# df_q3_0.wide <- df_q3_0.wide %>% mutate(
#   hazard_benifits=paste(hazard, benifits, sep = ";"))
# df_q3_0.wide.rules <- df_q3_0.wide %>% dplyr::select(hazard_benifits) %>%
#   rename(items=hazard_benifits)

### --- 0.2 Preprocessing BY ID
# df_q3_0 <- df_cities.20 %>% filter(qstn == "3.0",
#                                    colname %in% c("Co-benefit area", "Climate hazards", "Action"))
# 
# df_q3_0.hazards <- df_q3_0 %>% filter(colname %in% c("Climate hazards"), resp!="") %>%
#   dplyr::select(id, rown, resp) %>% rename(hazard=resp) %>%
#   mutate(hazard=unlist(sapply(strsplit(hazard, "\\ > "), "[", 1))) %>%
#   group_by(id) %>%
#   summarise_at(vars(hazard), funs(paste(., collapse = ';'))) # Merge to large groups
# 
# 
# df_q3_0.actions <- df_q3_0 %>% filter(colname %in% c("Action"), resp!="")
# df_q3_0.benifits <- df_q3_0 %>% filter(colname %in% c("Co-benefit area"), resp!="")
# 
# df_q3_0.actions <- df_q3_0.actions %>%
#   group_by(id) %>%
#   summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
#   ungroup() %>% rename(actions=resp)
# 
# df_q3_0.benifits <- df_q3_0.benifits %>%
#   group_by(id) %>%
#   summarise_at(vars(resp), funs(paste(., collapse = ';'))) %>%
#   ungroup() %>% rename(benifits=resp)
# 
# ### Version 1: Hazards, Actions, Benifits
# df_q3_0.wide <- df_q3_0.hazards %>%
#   left_join(df_q3_0.actions, by = "id") %>%
#   left_join(df_q3_0.benifits, by = "id")
# df_q3_0.wide <- df_q3_0.wide %>% filter(rowSums(is.na(df_q3_0.wide)) == 0)
# df_q3_0.wide <- df_q3_0.wide %>% mutate(
#   hazard_actions_benifits=paste(hazard, actions, benifits, sep = ";"))
# df_q3_0.wide.rules <- df_q3_0.wide %>% dplyr::select(hazard_actions_benifits) %>%
#   rename(items=hazard_actions_benifits)

### Version 3
# TODO: Separately by type of hazard

### Association Rules
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
export(df_q3_0.wide.rules, file = "df_q3_0.wide.rules.csv", "csv")
tr <- read.transactions("df_q3_0.wide.rules.csv", format = 'basket', sep=';')
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5, maxlen=3))
plot(association.rules,method="two-key plot")
# subRules <- association.rules[quality(association.rules)$confidence>0.9]
topSubRules <- head(association.rules, n = 20, by = "count")
topSubRules <- head(association.rules, n = 20, by = "support")
topSubRules <- head(association.rules, n = 20, by = "confidence")
topSubRules <- head(association.rules, n = 20, by = "lift")
topSubRules <- head(association.rules, n = 20, by = c("confidence", "lift"))
plot(topSubRules, method = "graph",  engine = "htmlwidget")


### OLD CODE

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

### --- 4. Addaptation Goals 


#############################################################################################################
############## Cities Population, 2020
#############################################################################################################

# df_q0_1 <- df_cities.20 %>% filter(qstn == "0.1", coln == 1) %>% dplyr::select(org, resp)
# t <- as.data.frame(table(df_q0_1$resp))

# setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
# export(df_q0_5, "df_q0_5.coded.xlsx", "xlsx")

df_q0_5 <- df_cities.20 %>% filter(qstn == "0.5", coln == 1) %>% dplyr::select(id, org, resp) %>%
  mutate(resp=as.numeric(resp))

df_q0_5[df_q0_5$id == "841098",]$resp <- 338000
df_q0_5[df_q0_5$id == "60318",]$resp <- 494000
df_q0_5[df_q0_5$id == "54652",]$resp <- 676000
df_q0_5[df_q0_5$id == "839650",]$resp <- 124100
df_q0_5[df_q0_5$id == "826381",]$resp <- 4412000
df_q0_5[df_q0_5$id == "841003",]$resp <- 524700
df_q0_5[df_q0_5$id == "826211",]$resp <- 399724
df_q0_5[df_q0_5$id == "42388",]$resp <- 1383432
df_q0_5[df_q0_5$id == "845309",]$resp <- 329675









