
# Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

###### Data
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Cities/Cities Responses'))
full_cities20 <- read.csv('2020_Full_Cities_Dataset.csv') %>% 
  select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
         'region' = CDP.Region, 'qstn' = Question.Number,
         'qstname' = Question.Name, 'coln' = Column.Number, 'colname' = Column.Name,
         'rown' = Row.Number, 'rowname' = Row.Name, 'resp' = Response.Answer)
# names(full_cities20)

subset <- c("3.2",	"3.2a",	"3.2a",	"3.2a",	"3.2a",	"3.3",	"4.0",	"4.15",	"6.2a",
            "6.2a",	"6.2",	"5.0",	"5.0a",	"5.0a",	"5.0a",	"5.0a",	"5.0c",	"5.0d",
            "5.0b",	"5.2",	"5.3",	"5.4",	"5.4",	"5.4",	"5.4",	"5.5",	"5.5a",
            "5.5a",	"5.5a",	"5.5a",	"5.5a",	"5.5a",	"10.7a",	"0.5",	"0.6",	"1.0",
            "1.0a",	"6.0",	"6.5",	"6.5",	"6.5",	"8.0",	"8.0a",	"8.1",	"8.2",
            "8.4",	"8.5",	"8.5a",	"8.5a",	"2.0",	"2.0b",	"2.0b",	"2.0b",	"2.3a",
            "14.1",	"12.4",	"10.7",	"13.0",	"14.3",	"14.3",	"14.3",	"14.4",	"14.4a", '2.1')

cities20 <- full_cities20 %>% filter(qstn %in% subset) 
length(unique(cities20$id))

#cities20 <- cities20[!(cities20$qstn == 'Response Language' & !cities20$resp == 'English'),]
#table(as.character(cities20[cities20$qstn == 'Response Language', 'resp']))

#######################################################  General info ####################################################### 
###### Q0.5: Population
cities20_ <- cities20 %>% select(id, org, qstn, coln, 'popsize' = resp) %>% filter(qstn == 0.5 & coln == 1)
###### Q0.6: geography
cities20_ <- cities20_ %>%
  full_join(cities20 %>% select(id, org, qstn, coln, 'landarea' = resp) %>% 
              filter(qstn == 0.6 & coln == 1) %>% select(id, org, landarea),  by = c('id', 'org')) 

####################################################### Governance ####################################################### 
###### Q1.0a: N of sustainability goals
cities20_ <- cities20_ %>%
  full_join(cities20 %>% select(id, org, qstn, coln, resp) %>% 
              filter(qstn == "1.0a" & coln == 1) %>% group_by(id, org, qstn) %>%
              summarise(sustaingoals = n()) %>%
              select(id, org, sustaingoals),  by = c('id', 'org')) %>%
              mutate(sustaingoals = replace_na(sustaingoals, 0))

#######################################################  Risk assessment ####################################################### 
###### Q2.0b
# Selecting relevant columns
Q2.0b <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '2.0b' & coln %in% c(1,4,7,8)) %>% select(id, org, coln, resp)
table(as.character(Q2.0b[Q2.0b$coln == 4,'resp']))

###### Boundary of assessment
boundary_question_recode <- function(df){
  boundary_vec <- as.data.frame(table(as.character(df)))
  boundary_vec <- as.character(boundary_vec$Var1)
  # replacing categrical values from a vector by ordinal values
  df <- ifelse(df == boundary_vec[str_detect(boundary_vec, "^Partial")], 2,
               ifelse(df == boundary_vec[str_detect(boundary_vec, "^Larger")], 4,
                      ifelse(df == boundary_vec[str_detect(boundary_vec, "^Smaller")], 1,
                             ifelse(df == boundary_vec[str_detect(boundary_vec, "^Same")], 3, 0))))
} # A function for recoding categorical values
Q2.0b[Q2.0b$coln == 4,'resp'] <- boundary_question_recode(Q2.0b[Q2.0b$coln == 4,'resp'])
Q2.0b_4 <- Q2.0b %>% group_by(id, org) %>% filter(coln == 4) %>% group_by(id, org) %>% 
  rename(boundary_assess = resp) %>% 
  summarise(boundary_assess = sum(as.numeric(as.character(boundary_assess)))) 

###### Vulnerable populations (at least in 1 assessment)
table(as.character(Q2.0b[Q2.0b$coln == 7,'resp']))
Q2.0b[Q2.0b$coln == 7,'resp'] <- recode(Q2.0b[Q2.0b$coln == 7,'resp'], 'Yes' = 1, 'No' = 0, .default = 0)
Q2.0b_7 <- Q2.0b %>% filter(coln == 7) %>% select(id, org, 'vulnpop_assess' = resp) %>% group_by(id, org) %>%
  summarise(vulnpop_assess = sum(as.numeric(as.character(vulnpop_assess))))

###### Number of assessment publications and areas/sectors covered in total
# Creating a vector with responses for the areas covered
areas_vec <- as.data.frame(table(as.character(Q2.0b[Q2.0b$coln == 8,'resp']))) %>% 
  filter(Freq > 20) %>% filter(!Var1 == "Question not applicable" & !Var1 == "") %>%
  select(Var1) 

# Counting number of rows with responses
Q2.0b_18 <- Q2.0b %>% filter(coln %in% c(1,8)) %>%
  filter((coln == 8 & resp %in% as.character(areas_vec$Var1) |
          str_detect(resp, "^Other, please specify")) | coln == 1) %>%
  group_by(id, org, coln) %>% 
  summarise(nprog = n()) %>% 
  pivot_wider(id_cols = c(id, org), names_from = coln, values_from = nprog, values_fill = list(nprog = NA)) %>%
  rename(n_assess = `1`, areas_assess = `8`)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q2.0b_4, Q2.0b_18, Q2.0b_7), by = c('id', 'org'), type='full')
length(unique(cities20_t$id))

####################################################### Social equity ####################################################### 
###### Q2.1 and Q3.2a_4: Number of hazards for vulnerable populations adressed in plans
Q2.1 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '2.1' & coln %in% c(1,7)) %>% select(id, org, coln, rown, resp) %>% arrange(org, coln, rown)

tt <- pivot_wider(Q2.1, id_cols = c(id, org), names_from = coln, values_from = resp)

####################################################### Adaptation targets ####################################################### 
###### Q3.2a: plans that addresses climate change adaptation
# Selecting relevant columns
Q3.2a <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '3.2a' & coln %in% c(1,3,4,6,8,9,10)) %>% select(id, org, coln, resp)

###### Number of documents

# A function counting number of rows with responses
count_rows <- function(subset, n, resp_name, select_from = F, categories){
  if (select_from == T){
    result <- subset %>% filter(coln %in% n) %>%
      filter(!resp == "Question not applicable" & !resp == "") %>% 
      filter(resp %in% {{categories}}) %>%
      group_by(id, org) %>% summarise({{resp_name}} := n())
  }
  else {
    result <- subset %>% filter(coln %in% n) %>%
      filter(!resp == "Question not applicable" & !resp == "") %>%
      group_by(id, org) %>% summarise({{resp_name}} := n())    
  }
  
  
  return(result)
}
Q3.2a_1 <- count_rows(Q3.2a, 1, n_doc)

##### Number of areas covered by plan
Q3.2a_3_categories <- c('Energy',
                        'Transport (mobility)',
                        'Building and Infrastructure',
                        'Industry',
                        'ICT (Information and Communication Technology)',
                        'Spatial Planning',
                        'Agriculture and Forestry',
                        'Fishery',
                        'Water',
                        'Waste',
                        'Public Health and Safety',
                        'Business and Financial Service',
                        'Social Services')
Q3.2a_3 <- count_rows(Q3.2a, 3, n_areas_plan, select_from = T, categories = Q3.2a_3_categories)

##### Number of climate hazards factored into plan
Q3.2a_4_categories <- as.data.frame(table(as.character(Q3.2a[Q3.2a$coln == 4,'resp']))) %>% select(Var1) %>%
  filter(grepl('>', Var1))
Q3.2a_4_categories <- as.character(Q3.2a_4_categories$Var1)
Q3.2a_4 <- count_rows(Q3.2a, 4, n_hazards_plan, select_from = T, categories = Q3.2a_4_categories)

###### Boundary of plan
Q3.2a[Q3.2a$coln == 6,'resp'] <- boundary_question_recode(Q3.2a[Q3.2a$coln == 6,'resp'])
table(as.character(Q3.2a[Q3.2a$coln == 6,'resp']))

# A function to sum values for ordinal multiple response questions
sum_for_ordinal_multiple_responses <- function(subset, n, resp_name){
  result <- subset %>% group_by(id, org) %>% filter(coln == n) %>%  
  summarise({{resp_name}} := sum(as.numeric(as.character(resp))))
  
  return(result)

}

# Summing values of a question about boundaries for all plans
Q3.2a_6 <- sum_for_ordinal_multiple_responses(Q3.2a, 6, boundary_plan)


###### Stage of implementation
stage_question_recode <- function(df){
# replacing categrical values from a vector by ordinal values
  df <- ifelse(df == 'Plan in development', 1,
        ifelse(df == 'Plan developed but not implemented', 2,
        ifelse(df == 'Plan in implementation', 3,
        ifelse(df == 'Implementation complete', 4,
        ifelse(df == 'Monitoring and evaluation in progress' | df == 'Measurement in progress', 5,
        ifelse(df == 'Plan update in progress', 6,
        ifelse(df %in% as.character(str_subset(df, "^Other, please specify")), 3, 0)))))))
} # A function to recode stage-related questions

# Recoding
Q3.2a[Q3.2a$coln == 8,'resp'] <- stage_question_recode(Q3.2a[Q3.2a$coln == 8,'resp'])
table(as.character(Q3.2a[Q3.2a$coln == 8,'resp'])) # checking the recoding results

# Summing values of the question about a stage for all plans
Q3.2a_8 <- sum_for_ordinal_multiple_responses(Q3.2a, 8, stage_plan)


###### Type of plan
# Recoding to ordinal scale
Q3.2a[Q3.2a$coln == 9,'resp'] <- ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Standalone', 1,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Integrated mitigation / adaptation', 2,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Addressed in general city master plan', 3,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Addressed in city sector plan', 4,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] %in% 
                                          as.character(str_subset(Q3.2a[Q3.2a$coln == 9,'resp'], "^Other, please specify")), 2, 0)))))
table(as.character(Q3.2a[Q3.2a$coln == 9,'resp'])) # checking the recoding results

# Summing values of the question about local government's assessment for all plans
Q3.2a_9 <- sum_for_ordinal_multiple_responses(Q3.2a, 9, plan_type)

###### Assessment by the local government

# A function to recode presence-absence-type categorial questions into ordinal
presence_or_absence_recode <- function(df){
  df <- ifelse(df == 'Yes', 3,
               ifelse(df == 'In Progress', 2,
                      ifelse(df == 'Intending to undertake in the next 2 years', 1,
                             ifelse(df == 'Not intending to undertake', 0,
                                    ifelse(df == "Donâ€™t know", 0, 0)))))
}

# Recoding
Q3.2a[Q3.2a$coln == 10,'resp'] <- presence_or_absence_recode(Q3.2a[Q3.2a$coln == 10,'resp'])
table(as.character(Q3.2a[Q3.2a$coln == 10,'resp'])) # checking the recoding results

# Summing values of the question about local government's assessment for all plans
Q3.2a_10 <- sum_for_ordinal_multiple_responses(Q3.2a, 10, localgov_assess_plan)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q3.2a_1, Q3.2a_3, Q3.2a_4, Q3.2a_6, Q3.2a_8,
                                 Q3.2a_9, Q3.2a_10), by = c('id', 'org'), type='full')
length(unique(cities20_t$id))


####################################################### 


