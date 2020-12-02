
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
library(arules)
library(arulesViz)
library(reshape2)
library(quantmod)
library(visNetwork)

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
############## Cities, Q3.0, 2019-2020, Hazards, Action, Co-benifits: Association Rules
#############################################################################################################

############## Cities, Q3.0, 2020, Hazards, Action, Co-benifits: Association Rules

# Loading
df_q3_0 <- df_cities.20 %>% filter(qstn == "3.0",
                                   colname %in% c("Co-benefit area", "Climate hazards", "Action")) %>%
  mutate(resp=gsub("â€“", " - ", resp))


df_q3_0.hazards <- df_q3_0 %>% filter(colname %in% c("Climate hazards"), resp!="") %>%
  dplyr::select(id, rown, resp) %>% rename(hazard=resp) %>%
  mutate(hazard=str_replace(hazard, "\\> ", "")) %>% 
  filter(hazard!="Question not applicable")
df_q3_0.actions <- df_q3_0 %>% filter(colname %in% c("Action"), !(resp %in% c("", "Other"))) %>%
  rename(actions=resp)
df_q3_0.benifits <- df_q3_0 %>% filter(colname %in% c("Co-benefit area"), !(resp %in% c("", "Other"))) %>%
  rename(benifits=resp)

# Save freq tables for the future
table.hazards.20 <- data.frame(table(df_q3_0.hazards$hazard))
table.actions.20 <- data.frame(table(df_q3_0.actions$actions))
table.benifits.20 <- data.frame(table(df_q3_0.benifits$benifits))

# Aggregate actions and co-benifits by id-rown
df_q3_0.actions <- df_q3_0.actions %>%
  group_by(id, rown) %>%
  summarise_at(vars(actions), funs(paste(., collapse = ';'))) %>%
  ungroup() 
df_q3_0.benifits <- df_q3_0.benifits %>%
  group_by(id, rown) %>%
  summarise_at(vars(benifits), funs(paste(., collapse = ';'))) %>%
  ungroup()

# Merge Hazards, Actions and Co-Benifits
df_q3_0.wide.20 <- df_q3_0.hazards %>%
  full_join(df_q3_0.actions, by = c("id", "rown")) %>%
  full_join(df_q3_0.benifits, by = c("id", "rown")) %>%
  filter(rowSums(is.na(.)) == 0) %>%
  mutate(items=paste(hazard, actions, benifits, sep = ";")) %>%
  dplyr::select(items)


############## Cities, Q3.0, 2019, Hazards, Action, Co-benifits: Association Rules

# Loading
df_q3_0 <- df_cities.19 %>% filter(qstn == "3.0",
                                   colname %in% c("Co-benefit area", "Climate hazards", "Action")) %>% 
  mutate(resp=gsub("â€“", " - ", resp))

df_q3_0.hazards <- df_q3_0 %>% filter(colname %in% c("Climate hazards"), resp!="") %>%
  dplyr::select(id, rown, resp) %>% rename(hazard=resp) %>%
  mutate(hazard=str_replace(hazard, "\\> ", "")) %>% 
  filter(hazard!="Question not applicable")
df_q3_0.actions <- df_q3_0 %>% filter(colname %in% c("Action"), !(resp %in% c("", "Other"))) %>%
  rename(actions=resp)
df_q3_0.benifits <- df_q3_0 %>% filter(colname %in% c("Co-benefit area"), !(resp %in% c("", "Other"))) %>%
  rename(benifits=resp)

# Save freq tables for the future
table.hazards.19 <- data.frame(table(df_q3_0.hazards$hazard))
table.actions.19 <- data.frame(table(df_q3_0.actions$actions))
table.benifits.19 <- data.frame(table(df_q3_0.benifits$benifits))

# Aggregate actions and co-benifits by id-rown
df_q3_0.actions <- df_q3_0.actions %>%
  group_by(id, rown) %>%
  summarise_at(vars(actions), funs(paste(., collapse = ';'))) %>%
  ungroup() 
df_q3_0.benifits <- df_q3_0.benifits %>%
  group_by(id, rown) %>%
  summarise_at(vars(benifits), funs(paste(., collapse = ';'))) %>%
  ungroup()

# Merge Hazards, Actions and Co-Benifits
df_q3_0.wide.19 <- df_q3_0.hazards %>%
  full_join(df_q3_0.actions, by = c("id", "rown")) %>%
  full_join(df_q3_0.benifits, by = c("id", "rown")) %>%
  filter(rowSums(is.na(.)) == 0) %>%
  mutate(items=paste(hazard, actions, benifits, sep = ";")) %>%
  dplyr::select(items)


############## 2019 and 2020 together

# Combine main datasets
df_q3_0.wide.rules <- rbind(df_q3_0.wide.19, df_q3_0.wide.20)
df_q3_0.wide.rules <- df_q3_0.wide.rules %>% distinct(items, .keep_all = T)
temp_vec <- df_q3_0.wide.rules$items
Encoding(temp_vec) <- "UTF-8"
df_q3_0.wide.rules$items <- temp_vec

# Combine tables
table.actions <- rbind(table.actions.19, table.actions.20) %>% mutate(Var1=as.character(Var1))
table.benifits <- rbind(table.benifits.19, table.benifits.20) %>% mutate(Var1=as.character(Var1))
table.hazards <- rbind(table.hazards.19, table.hazards.20) %>% mutate(Var1=as.character(Var1))

############## Association Rules
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
export(df_q3_0.wide.rules, file = "df_q3_0.wide.rules.csv", "csv")
tr <- read.transactions("df_q3_0.wide.rules.csv", format = 'basket', sep=';')

# Model
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5, minlen=3, maxlen=3))
association.rules
# Remove redundant rules. For example remove rule {A, B} -> {C}, if we have rule {A, B, D} -> {C}
association.rules <- association.rules[!is.redundant(association.rules)]
association.rules

# Remove association rules with Hazards in RHS
for (hazard in table.hazards$Var1){
  
  association.rules <- subset(association.rules,  subset = (!rhs %pin% hazard))
  print(association.rules)
  
}

topSubRules <- head(association.rules, n = 50, by = c("confidence", "lift")) # OK



ig <- plot(topSubRules, method="graph", control=list(type="items") )

# saveAsGraph seems to render bad DOT for this case
tf <- tempfile( )
saveAsGraph(topSubRules, file = tf, format = "dot" )
# clean up temp file if desired
#unlink(tf)

# let's bypass saveAsGraph and just use our igraph
ig_df <- get.data.frame(ig, what = "both")
ig_df$vertices$label <- gsub("â€“", "–", ig_df$vertices$label)

# Make a variable with a node type
ig_df$vertices$node_type <- ifelse(ig_df$vertices$label %in% table.actions$Var1, "Action",
                                   ifelse(ig_df$vertices$label %in% table.hazards$Var1, "Hazard",
                                          ifelse(ig_df$vertices$label %in% table.benifits$Var1, "Benifit",
                                                 "Rule")))
nodes <- data.frame(
  group = ig_df$vertices$node_type,
  id = ig_df$vertices$name,
  value = ig_df$vertices$support, # could change to lift or confidence
  title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label),
  ig_df$vertices)
nodes$color <- ifelse(nodes$label %in% table.actions$Var1, "blue",
                      ifelse(nodes$label %in% table.hazards$Var1, "darkred",
                             ifelse(nodes$label %in% table.benifits$Var1, "green",
                                    "lightgray")))
# Save for plotting
setwd("C:/Users/Artem/YandexDisk/CDP/data/for_plots")
saveRDS(ig_df, "ig_df")
saveRDS(nodes, "nodes")

# VisNetwork
visNetwork(
  nodes,
  edges = ig_df$edges
) %>%
  visEdges(arrows = "to") %>%
  # visNodes(color = node_colors) %>%
  visOptions( highlightNearest = T)

# TODO: Visualization by groups https://datastorm-open.github.io/visNetwork/groups.html


#############################################################################################################
############## TODO
#############################################################################################################

# 1. 2020 or 2019 + 2020?
# 2. Visualize with igraph https://bl.ocks.org/timelyportfolio/762aa11bb5def57dc27f



