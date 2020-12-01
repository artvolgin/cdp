
### ~~~ VISUALIZE HAZARDS ON THE MAP

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
library(maditr)
# Network
library(igraph)
library(tnet)
# Maps
library(sf)
library(raster)
library(dplyr)
library(spData)
library(sp)
library(grid)
library(rworldmap)
library(weathermetrics)
# library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package


#############################################################################################################
############## Data Loading
#############################################################################################################

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

# --- Load the 2019 data
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Cities/Cities Responses'))
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
temp_vec <- df_cities.19$org
Encoding(temp_vec) <- "UTF-8"
df_cities.19$org <- temp_vec
# Recode 'Other' category
df_cities.19$resp[grepl("^Other, please specify", as.character(df_cities.19$resp))] <- "Other"

# --- Load the 2018 data
df_cities.18 <- read.csv('2018_Full_Cities_Dataset.csv') %>% 
  dplyr::select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
         'region' = CDP.Region, 'qstn' = Question.Number,
         'qstname' = Question.Name, 'coln' = Column.Number, 'colname' = Column.Name,
         'rown' = Row.Number, 'rowname' = Row.Name, 'resp' = Response.Answer)
# Recode to UTF-8
temp_vec <- df_cities.18$resp
Encoding(temp_vec) <- "UTF-8"
df_cities.18$resp <- temp_vec
temp_vec <- df_cities.18$colname
Encoding(temp_vec) <- "UTF-8"
df_cities.18$colname <- temp_vec
temp_vec <- df_cities.18$org
Encoding(temp_vec) <- "UTF-8"
df_cities.18$org <- temp_vec
# Recode 'Other' category
df_cities.18$resp[grepl("^Other, please specify", as.character(df_cities.18$resp))] <- "Other"


# --- Additional information about the Cities

# --- 1. Population
df_cities_info <- df_cities.20 %>% filter(qstn == "0.5", coln == 1) %>% dplyr::select(id, org, cntry, resp) %>%
  mutate(population=as.numeric(resp)) %>% dplyr::select(-c(resp))
df_cities_info[df_cities_info$id == "841098",]$population <- 338000
df_cities_info[df_cities_info$id == "60318",]$population <- 494000
df_cities_info[df_cities_info$id == "54652",]$population <- 676000
df_cities_info[df_cities_info$id == "839650",]$population <- 124100
df_cities_info[df_cities_info$id == "826381",]$population <- 4412000
df_cities_info[df_cities_info$id == "841003",]$population <- 524700
df_cities_info[df_cities_info$id == "826211",]$population <- 399724
df_cities_info[df_cities_info$id == "42388",]$population <- 1383432
df_cities_info[df_cities_info$id == "845309",]$population <- 329675
# df_cities_info$population_log <- log(df_cities_info$population)

# --- 2. Parse coordinates
# register_google(key = "AIzaSyBXXfPn9HfpPLzLQUyYjMVoUwvN7MRgNnk")
# df_cities_coords <- geocode(df_cities_info$org)
setwd("C:/Users/Artem/YandexDisk/CDP/data")
# saveRDS(df_cities_coords, "df_cities_coords")
df_cities_coords <- readRDS("df_cities_coords")
df_cities_info <- cbind(df_cities_info, df_cities_coords)

# Add missing coordinates
df_cities_info[df_cities_info$id == 826210,]$lon <- -104.642256
df_cities_info[df_cities_info$id == 826210,]$lat <- 19.488188
df_cities_info[df_cities_info$id == 847922,]$lon <- -103.579368
df_cities_info[df_cities_info$id == 847922,]$lat <- 20.230812
df_cities_info[df_cities_info$id == 31186,]$lon <- 128.681585
df_cities_info[df_cities_info$id == 31186,]$lat <- 35.226370
df_cities_info[df_cities_info$id == 31163,]$lon <- 28.986425
df_cities_info[df_cities_info$id == 31163,]$lat <- 41.011792
df_cities_info.coords <- df_cities_info %>% dplyr::select(id, lon, lat)

# --- 3. Add GDP
# setwd("C:/Users/Artem/YandexDisk/CDP/data")
# df_cities_gdp <- rio::import("cities_gdp_per_capita_wiki.xlsx")
# colnames(df_cities_gdp) <- c("metro_id", "metro_name", "gdp_per_capita")
# df_cities_gdp$metro_name <- gsub(" \\(Greater\\)", "", df_cities_gdp$metro_name)
# # Parse coordinates
# # register_google(key = "AIzaSyBXXfPn9HfpPLzLQUyYjMVoUwvN7MRgNnk")
# # df_cities_gdp_coords <- geocode(df_cities_gdp$metro_name)
# ("C:/Users/Artem/YandexDisk/CDP/data")
# # saveRDS(df_cities_gdp_coords, "df_cities_gdp_coords")
# df_cities_gdp_coords <- readRDS("df_cities_gdp_coords")
# df_cities_gdp <- cbind(df_cities_gdp, df_cities_gdp_coords)
# df_cities_gdp <- df_cities_gdp %>% filter(!is.na(lon))
# 
# # Spatial join to main database
# df_cities_info <- df_cities_info %>% st_as_sf(coords=c("lon", "lat"))
# st_crs(df_cities_info) <- 4326
# df_cities_gdp <- df_cities_gdp %>% st_as_sf(coords=c("lon", "lat"))
# st_crs(df_cities_gdp) <- 4326
# df_cities_info <- st_join(x = df_cities_info, y = df_cities_gdp,
#         join = st_is_within_distance, dist=1000)


# --- 4. GDP
# Export for coding
# df_cities_info <- data.frame(df_cities_info) %>% dplyr::select(-geometry)
# export(df_cities_info_, file = "df_cities_info.xlsx", format = "xlsx")
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
df_cities_info <- rio::import("df_cities_info.coded.xlsx")
# df_cities_coords <- readRDS("df_cities_coords")
df_cities_info <- df_cities_info %>% left_join(df_cities_info.coords)
df_cities_info <- df_cities_info %>% filter(population>=50000, !is.na(gdp_per_capita))

# --- 5. Weather
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
temp_raster <- raster("ERA5_mean_temp.tif") # ECMWF/ERA5/MONTHLY, mean_2m_air_temperature, mean 2019
precip_raster <- raster("ERA5_sum_precip.tif") # ECMWF/ERA5/MONTHLY, total_precipitation, sum 2019
df_cities_info <- df_cities_info %>% st_as_sf(coords=c("lon", "lat"))
df_cities_info$mean_temp <- raster::extract(temp_raster, df_cities_info)
df_cities_info$mean_temp <- kelvin.to.celsius(df_cities_info$mean_temp, round = 3)
df_cities_info$sum_precip <- raster::extract(precip_raster, df_cities_info)

# --- 6. NO2 values
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
no2_raster <- raster("NO2_sum.tif") # COPERNICUS/S5P/OFFL/L3_NO2, NO2_column_number_density, sum 2019
df_cities_info$no2_sum <- raster::extract(no2_raster, df_cities_info)


# --- 7. Cities Population, 2020

# df_q0_1 <- df_cities.20 %>% filter(qstn == "0.1", coln == 1) %>% dplyr::select(org, resp)
# t <- as.data.frame(table(df_q0_1$resp))

# setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
# export(df_q0_5, "df_q0_5.coded.xlsx", "xlsx")

# df_q0_5 <- df_cities.20 %>% filter(qstn == "0.5", coln == 1) %>% dplyr::select(id, org, resp) %>%
#   mutate(resp=as.numeric(resp))
# 
# df_q0_5[df_q0_5$id == "841098",]$resp <- 338000
# df_q0_5[df_q0_5$id == "60318",]$resp <- 494000
# df_q0_5[df_q0_5$id == "54652",]$resp <- 676000
# df_q0_5[df_q0_5$id == "839650",]$resp <- 124100
# df_q0_5[df_q0_5$id == "826381",]$resp <- 4412000
# df_q0_5[df_q0_5$id == "841003",]$resp <- 524700
# df_q0_5[df_q0_5$id == "826211",]$resp <- 399724
# df_q0_5[df_q0_5$id == "42388",]$resp <- 1383432
# df_q0_5[df_q0_5$id == "845309",]$resp <- 329675


#############################################################################################################
############## Preprocessing
#############################################################################################################

# --- Hazards, 2020
df_hazards.20 <- df_cities.20 %>%
  filter(qstn == "2.1", coln %in% c(1, 2, 3, 4, 8, 9, 10, 11)) %>%
  dcast(id + rown ~ colname, value.var="resp")
colnames(df_hazards.20) <- str_replace_all(colnames(df_hazards.20), " ", "_")
colnames(df_hazards.20) <- str_replace_all(colnames(df_hazards.20), "\\?", "")

# Cleaning
df_hazards.20 <- df_hazards.20 %>%
  filter(Climate_Hazards!="",
         !(Current_probability_of_hazard %in% c("", "Does not currently impact the city", "Do not know")),
         !(Current_magnitude_of_hazard %in% c("", "Does not currently impact the city", "Do not know"))) %>%
  mutate(Current_magnitude_of_hazard=dplyr::recode(Current_magnitude_of_hazard,
                                                   "High"=5, "Medium High"=4, "Medium"=3,
                                                   "Medium Low"=2, "Low"=1),
         Current_probability_of_hazard=dplyr::recode(Current_probability_of_hazard,
                                                   "High"=5, "Medium High"=4, "Medium"=3,
                                                   "Medium Low"=2, "Low"=1),)
# Merge to large groups
df_hazards.20 <- df_hazards.20 %>%
  mutate(Climate_Hazards=unlist(sapply(strsplit(Climate_Hazards, "\\ > "), "[", 1)),
         Climate_Hazards=dplyr::recode(Climate_Hazards, "Biological hazards "="Biological hazards"))


# Adding social/services impact
df_hazards.20.5 <- df_cities.20 %>%
  filter(qstn == "2.1", coln == 5, resp != "") %>% group_by(id, rown) %>% summarise(Social_impact_n=n())
df_hazards.20.6 <- df_cities.20 %>%
  filter(qstn == "2.1", coln == 6, resp != "") %>% group_by(id, rown) %>% summarise(Services_impact_n=n())
df_hazards.20 <- df_hazards.20 %>% left_join(df_hazards.20.5) %>% left_join(df_hazards.20.6)
df_hazards.20 <- df_hazards.20 %>%
         mutate(Index_Current_impact=(Current_magnitude_of_hazard*Current_probability_of_hazard))
# Calculate sum Index of Current Impact for the cities
df_cities_info <- df_cities_info %>%
  left_join(df_hazards.20 %>% group_by(id) %>% summarise(mean_Index_Current_impact = mean(Index_Current_impact),
                                                         hazards_n=n()))
# Fill NAs with 0
df_cities_info <- df_cities_info %>% dplyr::mutate(mean_Index_Current_impact = replace_na(mean_Index_Current_impact, 0),
                                 hazards_n = replace_na(hazards_n, 0))

# Save to excel
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
rio::export(df_cities_info %>% data.frame() %>% dplyr::select(-geometry), "df_cities_info.additional.xlsx", "xlsx")


# --- Hazards, 2019
# df_hazards.19 <- df_cities.19 %>%
#   filter(qstn == "2.1", coln %in% c(1, 2, 3, 4, 6, 7, 8, 11)) %>%
#   dcast(id + rown ~ colname, value.var="resp")
# 
# # TODO
# 
# # --- Hazards, 2018
# df_hazards.18 <- df_cities.18 %>%
#   filter(qstn == "2.2a", coln %in% c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
#   dcast(id + rown ~ colname, value.var="resp")


#############################################################################################################
############## Map of Hazards
#############################################################################################################

# TODO: Interactive map


df_cities_info <- df_cities_info %>% st_as_sf(coords=c("lon", "lat"))
st_crs(df_cities_info) <- 4326

### --- Index of Current Impact

### World Map
tm_shape(world) +
  tm_polygons() + 
  tm_shape(df_cities_info) +
  tm_symbols(col = "mean_Index_Current_impact", border.col = "gray",
             size = "hazards_n", alpha = 0.75, scale = 2, palette = rev(RColorBrewer::brewer.pal(3,"RdYlGn")))

### US only
df_cities_info.us <- df_cities_info %>% filter(cntry == "United States of America")
# df_cities_info.us <- df_cities_info.us %>% filter(mean_Index_Current_impact!=max(mean_Index_Current_impact, na.rm=T))
hist(df_cities_info.us$mean_Index_Current_impact)
tm_shape(us_states) +
  tm_polygons() + 
  tm_shape(df_cities_info.us) +
  tm_symbols(col = "mean_Index_Current_impact", border.col = "gray",
             size = "hazards_n", alpha = 0.75, scale = 2, palette = rev(RColorBrewer::brewer.pal(3,"RdYlGn")))

### Europe
worldMap <- getMap()
# Select only the index of states member of the E.U.
europe_states <-worldMap[which(worldMap$SRES %in% c("Central and Eastern Europe (EEU)", "Western Europe (WEU)")),]
europe_states <- europe_states[!europe_states$NAME %in% c("S. Geo. and S. Sandw. Is.",
                                                          "Br. Indian Ocean Ter.", "Faroe Is.",
                                                          "Greenland"),]
europe_states <- st_as_sf(europe_states)
tm_shape(europe_states) +
  tm_polygons() + 
  tm_shape(df_cities_info) +
  tm_symbols(col = "mean_Index_Current_impact", border.col = "gray",
             size = "hazards_n", alpha = 0.75, scale = 3, palette = rev(RColorBrewer::brewer.pal(3,"RdYlGn")))


### --- Most Impactfull Hazard

df_hazards.20.maximpact <- df_hazards.20 %>%
  left_join(df_hazards.20 %>% group_by(id) %>% summarise(max_Index_Current_impact = max(Index_Current_impact))) %>%
  filter(Index_Current_impact==max_Index_Current_impact) 
df_hazards.20.maximpact <- df_hazards.20.maximpact %>% distinct(id, Climate_Hazards, .keep_all = TRUE)


# 1. Flood and sea level rise
flood_sea_hazards <- (df_hazards.20.maximpact %>% filter(Climate_Hazards == "Flood and sea level rise") %>% 
                        dplyr::select(id))$id
df_cities_info$flood_sea_hazards <- as.numeric(df_cities_info$id %in% flood_sea_hazards)

# 2. Extreme hot temperature
hot_temp_hazards <- (df_hazards.20.maximpact %>% filter(Climate_Hazards == "Extreme hot temperature") %>% 
                     dplyr::select(id))$id
df_cities_info$hot_temp_hazards <- as.numeric(df_cities_info$id %in% hot_temp_hazards)

# 3. Extreme Precipitation
extreme_precip_hazards <- (df_hazards.20.maximpact %>% filter(Climate_Hazards == "Extreme Precipitation") %>% 
                           dplyr::select(id))$id
df_cities_info$extreme_precip_hazards <- as.numeric(df_cities_info$id %in% extreme_precip_hazards)

### World Map
tm_shape(world) +
  tm_polygons() + 
  tm_shape(df_cities_info) +
  tm_symbols(col = "hot_temp_hazards", border.col = "gray",
             size = 1, alpha = 0.75, scale = 1, palette = "Reds")

### Europe
worldMap <- getMap()
# Select only the index of states member of the E.U.
europe_states <-worldMap[which(worldMap$SRES %in% c("Central and Eastern Europe (EEU)", "Western Europe (WEU)")),]
europe_states <- europe_states[!europe_states$NAME %in% c("S. Geo. and S. Sandw. Is.",
                                                          "Br. Indian Ocean Ter.", "Faroe Is.",
                                                          "Greenland"),]
europe_states <- st_as_sf(europe_states)
tm_shape(europe_states) +
  tm_polygons() + 
  tm_shape(df_cities_info) +
  tm_symbols(col = "flood_sea_hazards", border.col = "gray",
             size = 1, alpha = 0.75, scale = 1, palette = "Reds")


#############################################################################################################
############## Association Rules for Hazards
#############################################################################################################

df_hazards.20.rules <- df_hazards.20 %>%
  group_by(id) %>%
  summarise_at(vars(Climate_Hazards), funs(paste(., collapse = ';'))) %>%
  ungroup() %>% rename(items=Climate_Hazards) %>% dplyr::select(-c(id))

# Association Rules
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
export(df_hazards.20.rules, file = "df_hazards.20.rules.csv", "csv")
tr <- read.transactions("df_hazards.20.rules.csv", format = 'basket', sep=';')
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5, maxlen=3))
plot(association.rules,method="two-key plot")
# subRules <- association.rules[quality(association.rules)$confidence>0.9]
topSubRules <- head(association.rules, n = 20, by = "count")
topSubRules <- head(association.rules, n = 20, by = "support")
topSubRules <- head(association.rules, n = 20, by = "confidence")
topSubRules <- head(association.rules, n = 20, by = "lift")
topSubRules <- head(association.rules, n = 20, by = c("confidence", "lift", "support"))
plot(topSubRules, method = "graph",  engine = "htmlwidget")



