
# Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(quantmod)

# Some functions for the pre-processing
source("C:/GitHub/cdp/cdp-functions.R")

####################################################### 2020 #######################################################

###### Data
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Cities/Cities Responses'))
full_cities20 <- read.csv('2020_Full_Cities_Dataset.csv') %>% 
  select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
         'region' = CDP.Region, 'qstn' = Question.Number,
         'qstname' = Question.Name, 'coln' = Column.Number, 'colname' = Column.Name,
         'rown' = Row.Number, 'rowname' = Row.Name, 'resp' = Response.Answer)
# names(full_cities20)

subset <- c("3.2",	"3.2a",	"3.3",	"4.0",	"4.15",	"6.2a",
            "6.2a",	"6.2",	"5.0",	"5.0a",	"5.0c",	"5.0d",
            "5.0b",	"5.2",	"5.3",	"5.4",	"5.5",	"5.5a",
            "10.7a",	"0.5",	"0.6",	"1.0", '10.1','10.2',
            "1.0a",	"6.0",	"6.5",	"8.0",	"8.0a",	"8.1",
            "8.4",	"8.5",	"8.5a",	"2.0",	"2.0b",	"2.3a",
            "14.1",	"12.4",	"10.7",	"13.0",	"14.3",	"14.4",
            "14.4a", '2.1', '3.0', '10.4', '12.3',	"8.2", '0.4')

cities20 <- full_cities20 %>% filter(qstn %in% subset) 
#length(unique(cities20$id))

####################################################### General info 2020 ####################################################### 
###### Q0.5: Population
cities20_ <- cities20 %>% select(id, org, qstn, coln, 'popsize' = resp) %>%
  filter(qstn == 0.5 & coln == 1)
cities20_$popsize <- as.numeric(as.character(cities20_$popsize))
# Fixing incorrect responses
cities20_[cities20_$id == "841098",]$popsize <- 338000
cities20_[cities20_$id == "60318",]$popsize <- 494000
cities20_[cities20_$id == "54652",]$popsize <- 676000
cities20_[cities20_$id == "839650",]$popsize <- 124100
cities20_[cities20_$id == "826381",]$popsize <- 4412000
cities20_[cities20_$id == "841003",]$popsize <- 524700
cities20_[cities20_$id == "826211",]$popsize <- 399724
cities20_[cities20_$id == "42388",]$popsize <- 1383432
cities20_[cities20_$id == "845309",]$popsize <- 329675

###### Q0.6: geography
cities20_ <- cities20_ %>%
  full_join(cities20 %>% select(id, org, qstn, coln, 'landarea' = resp) %>% 
              filter(qstn == 0.6 & coln == 1) %>% select(id, org, landarea),  by = c('id', 'org')) 

####################################################### Governance 2020 ####################################################### 
###### Q1.0a
# Selecting relevant columns
Q1.0a <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '1.0a' & coln == 1) %>% select(id, org, coln, resp)
Q1.0a$resp <- as.character(Q1.0a$resp)

# Number of sustainability goals
Q1.0a_1 <- count_rows(Q1.0a, 1, sustain_goals)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q1.0a_1), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Risk assessment 2020 ####################################################### 
###### Q2.0b
# Selecting relevant columns
Q2.0b <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '2.0b' & coln %in% c(1,4,7,8)) %>% select(id, org, coln, resp)
Q2.0b$resp <- as.character(Q2.0b$resp)

###### Boundary of assessment
Q2.0b[Q2.0b$coln == 4,'resp'] <- boundary_question_recode(Q2.0b[Q2.0b$coln == 4,'resp'])
table(as.character(Q2.0b[Q2.0b$coln == 4,'resp']))

# Averaging for all assessments
Q2.0b_4 <- mean_for_multiple_responses(Q2.0b, 4, boundary_assess)

###### Vulnerable populations 
# Recoding
Q2.0b[Q2.0b$coln == 7,'resp'] <- ifelse(Q2.0b[Q2.0b$coln == 7,'resp'] == 'Yes', 1,
                                 ifelse(Q2.0b[Q2.0b$coln == 7,'resp'] == 'No', 0,
                                 ifelse(Q2.0b[Q2.0b$coln == 7,'resp'] == 'Question not applicable', 9999999, NA)))
table(as.character(Q2.0b[Q2.0b$coln == 7,'resp']))
      
# Averaging by all documents
Q2.0b_7 <- mean_for_multiple_responses(Q2.0b, 7, vulnpop_assess)

###### Number of assessment publications and areas/sectors covered in total
Q2.0b_1 <- count_rows(Q2.0b, 1, n_assess)

###### Number of areas/sectors covered in total
Q2.0b_8 <- count_rows(Q2.0b, 8, areas_assess)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q2.0b_4, Q2.0b_1, Q2.0b_8, Q2.0b_7), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Adaptation targets 2020 ####################################################### 
###### Q3.2a: plans that address climate change adaptation
# Selecting relevant columns
Q3.2a <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '3.2a' & coln %in% c(1,3,4,6,8,9,10)) %>% select(id, org, coln, resp)
Q3.2a$resp <- as.character(Q3.2a$resp)

###### Number of documents
Q3.2a_1 <- count_rows(Q3.2a, 1, n_doc_adaptation)

##### Number of areas covered by plans
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
Q3.2a_3 <- count_rows(Q3.2a, 3, n_areas_adaptation_plan, select_from = T, categories = Q3.2a_3_categories)

##### Number of climate hazards factored into plan
Q3.2a_4_categories <- as.data.frame(table(as.character(Q3.2a[Q3.2a$coln == 4,'resp']))) %>% select(Var1) %>%
  filter(grepl('>', Var1))
Q3.2a_4_categories <- as.character(Q3.2a_4_categories$Var1)
Q3.2a_4 <- count_rows(Q3.2a, 4, n_hazards_plan, select_from = T, categories = Q3.2a_4_categories)

###### Boundary of plan
Q3.2a[Q3.2a$coln == 6,'resp'] <- boundary_question_recode(Q3.2a[Q3.2a$coln == 6,'resp'])
table(as.character(Q3.2a[Q3.2a$coln == 6,'resp']))

# Averaging values of a question about boundaries for all plans
Q3.2a_6 <- mean_for_multiple_responses(Q3.2a, 6, boundary_adaptation_plan)


##### Stage of implementation
# Recoding
Q3.2a[Q3.2a$coln == 8,'resp'] <- stage_question_recode(Q3.2a[Q3.2a$coln == 8,'resp'])
table(as.character(Q3.2a[Q3.2a$coln == 8,'resp'])) # checking the recoding results

# Summing values of the question about a stage for all plans
Q3.2a_8 <- mean_for_multiple_responses(Q3.2a, 8, stage_adaptation_plan)


###### Type of plan
# Recoding to ordinal scale
Q3.2a[Q3.2a$coln == 9,'resp'] <- ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Standalone', 1,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Integrated mitigation / adaptation', 2,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Addressed in general city master plan', 3,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Addressed in city sector plan', 4,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] %in% 
                                          as.character(str_subset(Q3.2a[Q3.2a$coln == 9,'resp'], "^Other, please specify")), 2,
                                 ifelse(Q3.2a[Q3.2a$coln == 9,'resp'] == 'Question not applicable', 9999999, NA))))))
table(as.character(Q3.2a[Q3.2a$coln == 9,'resp'])) # checking the recoding results

# Average type of a plan
Q3.2a_9 <- mean_for_multiple_responses(Q3.2a, 9, adaptation_plan_type)

###### Assessment by the local government

# Recoding
Q3.2a[Q3.2a$coln == 10,'resp'] <- presence_or_absence_recode(Q3.2a[Q3.2a$coln == 10,'resp'])
table(as.character(Q3.2a[Q3.2a$coln == 10,'resp'])) # checking the recoding results

# Averaging values of the question about local government's assessment for all plans
Q3.2a_10 <- mean_for_multiple_responses(Q3.2a, 10, localgov_assess_adaptation_plan)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q3.2a_1, Q3.2a_3, Q3.2a_4, Q3.2a_6, Q3.2a_8,
                                 Q3.2a_9, Q3.2a_10), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Social equity 2020 ####################################################### 
###### Q3.0 Selecting hazards that were addressed by actions 
Q3.0 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.0' & coln %in% c(1,2)) %>% select(-qstn) %>% arrange(org, coln, rown)

Q3.0_2 <- Q3.0 %>% filter(coln %in% 2) %>%
  filter(!resp == "Question not applicable" & !resp == "" & !resp == 'No action currently taken') %>%
  left_join(Q3.0 %>% filter(coln %in% 1), by = c('id', 'org', 'rown')) %>%
  filter(!resp.y == '') %>% select(id, org, resp.y) %>% group_by(id, org) %>%
  mutate(resp = resp.y) %>% select(-resp.y)
Q3.0_2 <- distinct(Q3.0_2)

# Adding an indicator showing that a hazard was addressed
Q3.0_2$selected = 1

# Selecting all hazards reported
Q2.1 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '2.1' & coln %in% c(1,7)) %>% select(-qstn)%>% arrange(org, coln, rown)
Q2.1$resp <- as.character(Q2.1$resp)

# Merging the addressed hazards with all hazards reported
Q2.1_Q3.0_2 <- Q2.1 %>%
  left_join(Q3.0_2, by = c('id', 'org', 'resp')) %>% filter(selected == 1, coln == 1)

# Selecting reports with vulnerable populations and filtering only non-missing values
Q2.1_7 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '2.1' & coln == 7) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  filter(!resp == '')

# Merging the addressed hazards with their respective vulnerable populations
Q2.1_Q3.0_2_Q2.1_7 <- Q2.1_Q3.0_2 %>%
  left_join(Q2.1_7, by = c('id', 'org', 'rown'))

# Defining the number of vulnerable populations addressed
df_vuln_addressed <- Q2.1_Q3.0_2_Q2.1_7 %>% group_by(id, org) %>%
  filter(!is.na(resp.y)) %>% # filtering those who did not report on vulnerable populations for a certain hazard
  summarise(vuln_addressed = n(), .groups = 'drop')

# Defining the number of vulnerable populations in total
df_vuln_total <- Q2.1_7 %>% group_by(id, org) %>%
  summarise(vuln_total = n(), .groups = 'drop')

# Addressed per total
df_vuln_addressed_per_total <- df_vuln_addressed %>%
  left_join(df_vuln_total, by = c('id', 'org')) %>%
  filter(!is.na(vuln_total)) %>% 
  mutate(vuln_addressed_per_total = vuln_addressed/vuln_total) %>%
  select(id, org, vuln_addressed_per_total)
#hist(df_vuln_addressed_per_total$vuln_addressed_per_total)

###### Q2.3a how climate change impacts health outcomes and health services
#Q2.3a_2 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
#  filter(qstn == '2.3a' & coln == 2) %>% select(-qstn) %>% 
#  arrange(org, coln, rown) %>%
#  filter(!resp == "Question not applicable" & !resp == "")
#Q2.3a_2$resp <- as.character(Q2.3a_2$resp)

# Recoding
#Q2.3a_2$resp <- ifelse(Q2.3a_2$resp == 'Yes', 1, 0)

# Percentage of areas for which health-related risk and vulnerability assessment was undertaken
#Q2.3a_2 <- Q2.3a_2 %>% group_by(id, org) %>%
#  summarise(vuln_health_assessed = mean(resp), .groups = 'drop')
#hist(Q2.3a_2$vuln_health_assessed)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, df_vuln_addressed_per_total), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

# If a city does not have vulnerable populations, assign it 1
cities20_[is.na(cities20_$vuln_addressed_per_total),'vuln_addressed_per_total'] <- 1

####################################################### Adaptation Actions 2020 ####################################################### 
###### Q2.1 and Q3_1: Hazards in actions per hazards reported
Q3.0 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.0' & coln %in% c(1,2)) %>% select(-qstn) %>% arrange(org, coln, rown)

# Number of actions in total
Q3.0_1 <- count_rows(Q3.0, 1, n_hazards)

# Number of actions in total
Q3.0_2 <- Q3.0 %>% filter(coln %in% 2) %>%
  filter(!resp == "Question not applicable" & !resp == "" & !resp == 'No action currently taken') %>%
  group_by(id, org) %>% summarise(n_adaptation_actions = n(), .groups = 'drop') 

# Merging 
Q3.0_12 <- Q3.0_1 %>%
  full_join(Q3.0_2, by = c('id', 'org'))

# Number of UNIQUE hazards addressed
Q3.0_12$n_hazards_addressed <- apply(Q3.0_12[, c('n_hazards', 'n_adaptation_actions')], 1, FUN = min)
Q3.0_12$n_hazards_addressed <- ifelse(is.na(Q3.0_12$n_hazards) | is.na(Q3.0_12$n_adaptation_actions) |
                                        Q3.0_12$n_hazards == 9999999 |
                                        Q3.0_12$n_adaptation_actions == 9999999, 0, Q3.0_12$n_hazards_addressed)
Q3.0_12[Q3.0_12 == 9999999 | is.na(Q3.0_12)] <- 0

# Actions per hazard
Q3.0_12$n_actions_per_hazard <- Q3.0_12$n_adaptation_actions/Q3.0_12$n_hazards
Q3.0_12[is.infinite(Q3.0_12$n_actions_per_hazard) | is.nan(Q3.0_12$n_actions_per_hazard), 'n_actions_per_hazard'] <- NA

Q3.0_12 <- Q3.0_12 %>% select(id, org, n_hazards_addressed, n_actions_per_hazard)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q3.0_12), by = c('id', 'org'), type='full')

###### Q3.0
Q3.0 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.0' & coln %in% c(2,4,6,7)) %>% select(id, org, coln, rown, resp) %>% arrange(org, coln, rown)
Q3.0$resp <- as.character(Q3.0$resp)

######  Number of actions
Q3.0_2 <- count_rows(Q3.0, 2, n_adaptation_actions)

######  Status of actions (sum by all actions)
Q3.0[Q3.0$coln == 4, 'resp'] <- ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Scoping', 1,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Pre-feasibility study/impact assessment', 2,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Pre-implementation', 3,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Implementation', 4,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Implementation complete but not in operation', 5,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Operation', 6,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == "Monitoring and reporting", 7,
      ifelse(Q3.0[Q3.0$coln == 4, 'resp'] %in% as.character(str_subset(Q3.0[Q3.0$coln == 4, 'resp'], "^Other, please specify")), 3.5, NA))))))))
table(as.character(Q3.0[Q3.0$coln == 4, 'resp']))

# Averaging values of a question about status for all actions
Q3.0_4 <- mean_for_multiple_responses(Q3.0, 4, adaptation_action_status)

######  Number of co-benefits
Q3.0_6 <- count_rows(Q3.0, 6, n_adaptation_cobenefits)

######  Number of Sectors/areas adaptation actions apply to
Q3.0_7 <- count_rows(Q3.0, 7, n_adaptation_actions_areas)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q3.0_2, Q3.0_4, Q3.0_6, Q3.0_7), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Adaptation Goals 2020 ####################################################### 
###### Q3.3
Q3.3 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.3' & coln %in% c(1,2)) %>% select(id, org, coln, rown, resp) %>%
  arrange(org, coln, rown)

Q3.3$resp <- as.character(Q3.3$resp)

######  Number of goals
Q3.3_1 <- count_rows(Q3.3, 1, n_goals)

######  Number of climate hazards that adaptation goals address
Q3.3_2 <- count_rows(Q3.3, 2, n_hazards_goals)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q3.3_1, Q3.3_2), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Inventory 2020 ####################################################### 
###### Q4.0: a city-wide emissions inventory
Q4.0 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '4.0') %>% select(id, org, coln, rown, resp) %>% arrange(org, coln, rown)

# Recoding into ordinal numeric
Q4.0[,'resp'] <- presence_or_absence_recode(Q4.0[,'resp']) 
Q4.0 <- Q4.0 %>% select(id, org, 'inventory' = resp)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q4.0), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Emission Reduction 2020 ####################################################### 
###### Q5.0: % of target achieved and reduction target in metric tonnes
Q5.0a <- emission_reduction_targets(cities20, '5.0a', 9, 10)
Q5.0b <- emission_reduction_targets(cities20, '5.0b', 7, 8)
Q5.0c <- emission_reduction_targets(cities20, '5.0c', 11, 12)
Q5.0d <- emission_reduction_targets(cities20, '5.0d', 8, 10)

# cities with no target
Q5_no_target <- cities20 %>% select(id, org, qstn, resp) %>%
  filter(qstn == '5.0' & resp == 'No target') %>% 
  mutate(pcnt_target_achieved = 9999999,
         target_metric_tonnes = 9999999) %>% 
  select(-c(qstn, resp)) %>% distinct(.keep_all = T)
#length(unique(Q5_no_target$id))

# Combining all target types and summarising if a city fell into 2 categories
Q5.0abcd <- rbind.data.frame(Q5.0a, Q5.0b, Q5.0c, Q5.0d, Q5_no_target) %>% group_by(id, org) %>%
  summarise_all(list(sum))

##### Q5.4 Mitigation Targets
Q5.4 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '5.4' & coln %in% c(1,4:7,9)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q5.4$resp <- as.character(Q5.4$resp)

# Number of mitigation actions
Q5.4_1 <- count_rows(Q5.4, 1, n_mitigation_actions)

######  Status of actions (sum by all actions)
Q5.4[Q5.4$coln == 4, 'resp'] <- ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Scoping', 1,
                                ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Pre-feasibility study', 2,
                                ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Pre-implementation', 3,
                                ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Implementation', 4,
                                ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Implementation complete', 5,
                                ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Operation', 6,
                                ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == "Monitoring and reporting", 7, NA)))))))
table(as.character(Q5.4[Q5.4$coln == 4, 'resp']))

# Averaging values of a question about status for all actions
Q5.4_4 <- mean_for_multiple_responses(Q5.4, 4, mitigation_action_status)

######  Number of co-benefits
Q5.4_9 <- count_rows(Q5.4, 9, n_mitigation_cobenefits)

###### Estimated emissions reduction: sum for all actions per population size
Q5.4_5 <- sum_for_multiple_responses(Q5.4, 5, estimated_emissions_reduction)

###### Energy savings (MWh): sum for all actions per population size
Q5.4_6 <- sum_for_multiple_responses(Q5.4, 6, energy_savings)

###### Renewable energy production (MWh): sum for all actions per population size
Q5.4_7 <- sum_for_multiple_responses(Q5.4, 7, renewable_energy_production)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q5.0abcd, Q5.4_1, Q5.4_4, Q5.4_5, Q5.4_6,
                                 Q5.4_7, Q5.4_9), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

##### Q5.5a Mitigation Plan
Q5.5a <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '5.5a' & coln %in% c(1,5,6,8,9)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q5.5a$resp <- as.character(Q5.5a$resp)

######  Number of documents
Q5.5a_1 <- count_rows(Q5.5a, 1, n_doc_mitigation)

##### Number of areas covered by plan
Q5.5a_5 <- count_rows(Q5.5a, 5, n_areas_mitigation_plan)

###### Boundary of a plan
Q5.5a[Q5.5a$coln == 6,'resp'] <- boundary_question_recode(Q5.5a[Q5.5a$coln == 6,'resp'])
table(as.character(Q5.5a[Q5.5a$coln == 6,'resp']))

# Averaging values of a question about boundaries for all plans
Q5.5a_6 <- mean_for_multiple_responses(Q5.5a, 6, boundary_mitigation_plan)

###### Stage of implementation

# Recoding
Q5.5a[Q5.5a$coln == 8,'resp'] <- stage_question_recode(Q5.5a[Q5.5a$coln == 8,'resp'])
table(as.character(Q5.5a[Q5.5a$coln == 8,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all plans
Q5.5a_8 <- mean_for_multiple_responses(Q5.5a, 8, stage_mitigation_plan)

###### Assessment by the local government

# Recoding
Q5.5a[Q5.5a$coln == 9,'resp'] <- presence_or_absence_recode(Q5.5a[Q5.5a$coln == 9,'resp'])
table(as.character(Q5.5a[Q5.5a$coln == 9,'resp'])) # checking the recoding results

# Averaging values of the question about local government's assessment for all plans
Q5.5a_9 <- mean_for_multiple_responses(Q5.5a, 9, localgov_assess_mitigation_plan)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q5.5a_1, Q5.5a_5, Q5.5a_6,
                                 Q5.5a_8, Q5.5a_9), by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Opportunities 2020 ####################################################### 
###### Q6.0: Number of opportunities identified as a result of addressing climate change
Q6.0 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '6.0' & coln == 1) %>% select(-qstn) %>% arrange(org, coln, rown)
Q6.0$resp <- as.character(Q6.0$resp)

# Number of opportunities
Q6.0_1 <- count_rows(Q6.0, 1, n_opportunities)

###### Q6.2a: Collaboration with business
Q6.2a <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '6.2a' & coln %in% c(1,2)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q6.2a$resp <- as.character(Q6.2a$resp)

# Number of collaboration areas
Q6.2a_1 <- count_rows(Q6.2a, 1, n_collab_areas)

# Number of collaboration types
Q6.2a_2 <- count_rows(Q6.2a, 2, n_collab_types)

###### Q6.5: Collaboration with business
Q6.5 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '6.5' & coln %in% c(1,3,4)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q6.5$resp <- as.character(Q6.5$resp)

# Number of unique project areas
Q6.5_1 <- count_rows(Q6.5, 1, n_project_areas)

###### Stage of implementation

# Recoding
Q6.5[Q6.5$coln == 3,'resp'] <- ifelse(Q6.5[Q6.5$coln == 3,'resp'] == 'Scoping', 1,
                               ifelse(Q6.5[Q6.5$coln == 3,'resp'] == 'Pre-feasibility study/impact assessment', 2,
                               ifelse(Q6.5[Q6.5$coln == 3,'resp'] == 'Project feasibility', 3,
                               ifelse(Q6.5[Q6.5$coln == 3,'resp'] == 'Project structuring', 4,
                               ifelse(Q6.5[Q6.5$coln == 3,'resp'] == 'Transaction preparation', 5,
                               ifelse(Q6.5[Q6.5$coln == 3,'resp'] == 'Implementation', 6,
                               ifelse(Q6.5[Q6.5$coln == 3,'resp'] == "Post-implementation", 7,
                               ifelse(Q6.5[Q6.5$coln == 3, 'resp'] == 'Question not applicable', 9999999, NA))))))))
table(as.character(Q6.5[Q6.5$coln == 3,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all projects
Q6.5_3 <- mean_for_multiple_responses(Q6.5, 3, stage_project)

###### Status of financing
# Recoding
#Q6.5[Q6.5$coln == 4,'resp'] <- ifelse(Q6.5[Q6.5$coln == 4,'resp'] == 'Project not funded and seeking partial funding', 1,
#                               ifelse(Q6.5[Q6.5$coln == 4,'resp'] == 'Project not funded and seeking full funding', 2,
#                               ifelse(Q6.5[Q6.5$coln == 4,'resp'] == 'Project partially funded and seeking additional funding', 3,
#                               ifelse(df %in% as.character(str_subset(df, "^Other, please specify")), 1.5, 0))))
#t <- as.data.frame(table(as.character(Q6.5[Q6.5$coln == 4,'resp']))) # checking the recoding results

# Summing values of the question about a stage for all projects
#Q6.5_4 <- sum_for_multiple_responses(Q6.5, 4, fin_status_project)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q6.0_1, Q6.2a_1, Q6.2a_2, Q6.5_1, Q6.5_3),
                            by = c('id', 'org'), type='full')
#length(unique(Q6.5_3$id))
####################################################### Renewable Energy Target 2020 ####################################################### 
###### Q8.0a 
Q8.0a <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '8.0a' & coln %in% c(9,10)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q8.0a$resp <- as.character(Q8.0a$resp)
Q8.0a[Q8.0a$resp == 'Question not applicable', 'resp'] <- 9999999

# Average percentage of renewable energy / electricity of total energy or electricity in target year
Q8.0a_9 <- mean_for_multiple_responses(Q8.0a, 9, percentage_renewable_energy)

# Average percentage of targets achieved
Q8.0a_10 <- mean_for_multiple_responses(Q8.0a, 10, renewable_energy_achieved)

###### Q8.1 the source mix of electricity consumed
Q8.1 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '8.1' & coln %in% 5:9) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  group_by(id, org) %>%
  summarise(renewable_energy_source = ifelse(all(is.na(as.numeric(as.character(resp)))),
                                  NA, sum(as.numeric(as.character(resp)), na.rm = T)), .groups = 'drop')

####### Q8.5a city’s energy efficiency targets
Q8.5a <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '8.5a' & coln %in% c(7,8)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q8.5a$resp <- as.character(Q8.5a$resp)
Q8.5a[Q8.5a$resp == 'Question not applicable', 'resp'] <- 9999999

# Average percentage of energy efficiency improvement in target year compared to base year levels
Q8.5a_7 <- mean_for_multiple_responses(Q8.5a, 7, percentage_energy_efficiency_improvement)

# Average percentage of target achieved
Q8.5a_8 <- mean_for_multiple_responses(Q8.5a, 8, energy_efficiency_achieved)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q8.0a_9, Q8.0a_10, Q8.1, Q8.5a_7, Q8.5a_8),
                            by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Transport 2020 ####################################################### 
###### Q10.1 the mode share of each transport mode
Q10.1 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '10.1' & coln %in% c(5,6)) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  group_by(id, org) %>% filter(!resp == "Question not applicable") %>%
  summarise(percentage_walking_cycling = ifelse(all(is.na(as.numeric(as.character(resp)))),
                                             NA, sum(as.numeric(as.character(resp)), na.rm = T)), .groups = 'drop')
# Adding cities with 'not applicable" responses
Q10.1 <- rbind.data.frame(Q10.1, cities20 %>% filter(qstn == '10.1' & coln %in% c(5,6)) %>%
                             filter(resp == "Question not applicable") %>%
                             select(id, org) %>%
                             mutate(percentage_walking_cycling = 9999999)) %>% distinct(.keep_all = T)

###### Q10.4 Total number of vehicle types for private cars (Electric + Hybrid + Plug in hybrid + Hydrogen)
Q10.4 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '10.4' & coln == 1 & rown %in% 2:5) %>% select(-qstn) %>% arrange(org, coln, rown)%>%
  group_by(id, org) %>% filter(!resp == "Question not applicable") %>%
  summarise(eco_private_cars = ifelse(all(is.na(as.numeric(as.character(resp)))),
                                                NA, sum(as.numeric(as.character(resp)), na.rm = T)), .groups = 'drop')
# Adding cities with 'not applicable" responses
Q10.4 <- rbind.data.frame(Q10.4, cities20 %>% filter(qstn == '10.4' & coln == 1 & rown %in% 2:5) %>%
                            filter(resp == "Question not applicable") %>%
                            select(id, org) %>%
                            mutate(eco_private_cars = 9999999)) %>% distinct(.keep_all = T)

###### Q10.7a: zero emissions zone presence
Q10.7 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '10.7') %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  mutate(zero_emission_zone_presence = ifelse(resp == 'Yes', 1,
                                              ifelse(resp == 'No' | resp == 'Do not know', 0, NA))) %>%
  select(-c(coln, rown, resp))

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q10.1, Q10.4, Q10.7),
                            by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Food 2020 ####################################################### 
###### Q12.3: presence of policies relating to food consumption
Q12.3 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '12.3' & coln == 1) %>% select(-qstn) %>% arrange(org, coln, rown)%>%
  group_by(id, org) %>% mutate(food_policies_presence = ifelse(resp == 'Yes', 1, 
                                              ifelse(resp == 'No' | resp == 'Do not know', 0, NA)))%>%
  select(-c(coln, rown, resp))


# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q12.3),
                            by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Waste 2020 ####################################################### 
###### Q13.0: Amount of solid waste generated (tonnes/year) per population size
Q13.0 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '13.0' & coln == 1) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  mutate(waste_amount = as.numeric(as.character(resp))) %>%
  select(-c(coln, rown, resp))

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q13.0),
                            by = c('id', 'org'), type='full')
#length(unique(cities20_$id))

####################################################### Water Security 2020 ####################################################### 
###### Q14.3
Q14.3 <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '14.3' & coln %in% 2:3) %>% select(-qstn) %>% arrange(org, coln, rown)
Q14.3$resp <- as.character(Q14.3$resp)

# Number of unique adaptation actions
Q14.3_2 <- count_rows(Q14.3, 2, n_water_adaptation_actions)

# Status
Q14.3[Q14.3$coln == 3, 'resp'] <- ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Scoping', 1,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Pre-feasibility study/impact assessment', 2,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Pre-implementation', 3,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Implementation', 4,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Implementation complete but not in operation', 5,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Operation', 6,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == "Monitoring and reporting", 7,
                                  ifelse(Q14.3[Q14.3$coln == 3, 'resp'] == 'Question not applicable', 9999999, NA))))))))
table(as.character(Q14.3[Q14.3$coln == 3, 'resp']))

# Averaging values for all water adaptation actions
Q14.3_3 <- mean_for_multiple_responses(Q14.3, 3, water_adaptation_action_status)
length(unique(Q14.3_3$id))

###### Q14.4a: Amount of solid waste generated (tonnes/year)
Q14.4a <- cities20 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '14.4a' & coln %in% c(1,5)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q14.4a$resp <- as.character(Q14.4a$resp)

# Number of documents
Q14.4a_1 <- count_rows(Q14.4a, 1, n_water_management)

# Stage of implementation
Q14.4a[Q14.4a$coln == 5, 'resp'] <- stage_question_recode(Q14.4a[Q14.4a$coln == 5, 'resp'])
table(as.character(Q14.4a[Q14.4a$coln == 5,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all plans
Q14.4a_5 <- mean_for_multiple_responses(Q14.4a, 5, stage_water_management_plan)

# Merging with the selected variables so far
cities20_ <- plyr::join_all(list(cities20_, Q14.3_2, Q14.3_3, Q14.4a_1, Q14.4a_5),
                            by = c('id', 'org'), type='full')
#length(unique(cities20_$id))




####################################################### KPIs 2020 #######################################################
# Replacing 9999999 with 0
cities20_[cities20_ == 9999999] <- 0

# Norming by population size a range of items
cities20_$target_metric_tonnes <- cities20_$target_metric_tonnes/cities20_$popsize
cities20_$estimated_emissions_reduction <- cities20_$estimated_emissions_reduction/cities20_$popsize
cities20_$energy_savings <- cities20_$energy_savings/cities20_$popsize
cities20_$renewable_energy_production <- cities20_$renewable_energy_production/cities20_$popsize
cities20_$waste_amount <- cities20_$waste_amount/cities20_$popsize

# If waste is missing, replacing with a mean value
cities20_[is.na(cities20_$waste_amount), 'waste_amount'] <- mean(cities20_[!is.na(cities20_$waste_amount), 'waste_amount'])

# Exploring patterns of missing values
cities20_items_only <- cities20_[,c(1,2,7:ncol(cities20_))]
row_na <- cbind.data.frame(cities20_items_only$org,  'row_na_sum' = rowSums(is.na(cities20_items_only))) %>% arrange(row_na_sum)
col_na <- cbind.data.frame(colnames(cities20_items_only),  'col_na_sum' = colSums(is.na(cities20_items_only))) %>% arrange(col_na_sum)
rownames(col_na) <- NULL

# Removing variables with >200 missing values
cities20_items_only_ <- cities20_items_only %>% select(-(as.character(col_na[col_na$col_na_sum > 200,
                                           'colnames(cities20_items_only)'])))
# Recalculating missing patterns
row_na <- cbind.data.frame(cities20_items_only_$org,  'row_na_sum' = rowSums(is.na(cities20_items_only_))) %>% arrange(row_na_sum)
col_na <- cbind.data.frame(colnames(cities20_items_only_),  'col_na_sum' = colSums(is.na(cities20_items_only_))) %>% arrange(col_na_sum)
rownames(col_na) <- NULL

# Removing cities with >10 missing values and for the rest replacing missing values with 0
cities20_items_only_2 <- cities20_items_only_ %>% filter(! org %in% as.character(row_na[row_na$row_na_sum > 10,
                                                        'cities20_items_only_$org']))
cities20_items_only_2[is.na(cities20_items_only_2)] <- 0
#summary(cities20_items_only_2)

# Focusing only on cities with population size > 50000
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data'))
df_cities_info <- rio::import("df_cities_info.additional.xlsx") %>% 
  mutate(hazards_intensity = mean_Index_Current_impact*hazards_n,
         no2_per_population = no2_sum/population)
df_cities_info <- df_cities_info %>% filter(population >= 50000, !is.na(gdp_per_capita)) %>%
  select(id, org, gdp_per_capita, hazards_intensity,
         population, mean_temp, sum_precip, no2_per_population)

# Merging
cities20_items_only_2 <-df_cities_info %>%
  left_join(cities20_items_only_2 %>% select(-org), by = 'id')
cities20_items_only_2 <- na.omit(cities20_items_only_2)




####################################################### 2019 #######################################################

###### Data
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Cities/Cities Responses'))
full_cities19 <- read.csv('2019_Full_Cities_Dataset.csv') %>% 
  select('id' = Account.Number, 'org' = Organization, 'cntry' = Country,
         'region' = CDP.Region, 'qstn' = Question.Number,
         'qstname' = Question.Name, 'coln' = Column.Number, 'colname' = Column.Name,
         'rown' = Row.Number, 'rowname' = Row.Name, 'resp' = Response.Answer)
#names(full_cities19)

subset <- c("3.2",	"3.1a",	"3.3",	"4.0",	"4.15",	"6.1a",
            "6.2a",	"6.2",	"5.0",	"5.0a",	"5.0c",	"5.0d",
            "5.0b",	"5.2",	"5.3",	"5.4",	"5.5",	"5.5a",
            "10.7a",	"0.5",	"0.6",	"1.0", '10.1','10.2',
            "1.0a",	"6.0",	"6.2",	"8.0",	"8.0a",	"8.2",
            "8.4",	"8.5",	"8.6a",	"2.0",	"2.0b",	"2.3a",
            "14.1",	"10.7",	"13.0",	"14.4",
            "14.5a", '2.1', '3.0', '10.5', '12.4',	"8.2", '0.4')

cities19 <- full_cities19 %>% filter(qstn %in% subset) 

# Fixing 'Not applicable' coding
cities19$resp <- as.character(cities19$resp)
cities19[cities19$resp %in% c('Not applicable.',
                              'Not applicable',
                              'not applicable') & 
           is.na(cities19$resp) == F, 'resp'] <- 'Question not applicable'

####################################################### General info 2019 ####################################################### 
###### Q0.5: Population
cities19_ <- cities19 %>% select(id, org, qstn, coln, 'popsize' = resp) %>%
  filter(qstn == 0.5 & coln == 1)
cities19_$popsize <- as.numeric(as.character(cities19_$popsize))
# Fixing incorrect responses
cities19_[cities19_$id == "841098",]$popsize <- 338000
cities19_[cities19_$id == "60318",]$popsize <- 494000
cities19_[cities19_$id == "54652",]$popsize <- 676000
cities19_[cities19_$id == "839650",]$popsize <- 124100
#cities19_[cities19_$id == "826381",]$popsize <- 4412000
cities19_[cities19_$id == "841003",]$popsize <- 524700
#cities19_[cities19_$id == "826211",]$popsize <- 399724
#cities19_[cities19_$id == "42388",]$popsize <- 1383432
#cities19_[cities19_$id == "845309",]$popsize <- 329675

###### Q0.6: geography
cities19_ <- cities19_ %>%
  full_join(cities19 %>% select(id, org, qstn, coln, 'landarea' = resp) %>% 
              filter(qstn == 0.6 & coln == 1) %>% select(id, org, landarea),  by = c('id', 'org')) 

####################################################### Governance 2019 ####################################################### 
###### Q1.0a
# Selecting relevant columns
Q1.0a <- cities19 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '1.0a' & coln == 1) %>% select(id, org, coln, resp)
Q1.0a$resp <- as.character(Q1.0a$resp)

# Number of sustainability goals
Q1.0a_1 <- count_rows(Q1.0a, 1, sustain_goals)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q1.0a_1), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Risk assessment 2019 ####################################################### 
###### Q2.0b
# Selecting relevant columns
Q2.0b <- cities19 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '2.0b' & coln %in% c(1,4,6,8)) %>% select(id, org, coln, resp)
Q2.0b$resp <- as.character(Q2.0b$resp)

###### Boundary of assessment
Q2.0b[Q2.0b$coln == 4,'resp'] <- boundary_question_recode(Q2.0b[Q2.0b$coln == 4,'resp'])
table(as.character(Q2.0b[Q2.0b$coln == 4,'resp']))

# Averaging for all assessments
Q2.0b_4 <- mean_for_multiple_responses(Q2.0b, 4, boundary_assess)

###### Vulnerable populations 
# Recoding
Q2.0b[Q2.0b$coln == 8,'resp'] <- ifelse(Q2.0b[Q2.0b$coln == 8,'resp'] == 'Yes', 1,
                                        ifelse(Q2.0b[Q2.0b$coln == 8,'resp'] == 'No', 0,
                                               ifelse(Q2.0b[Q2.0b$coln == 8,'resp'] == 'Question not applicable', 9999999, NA)))
table(as.character(Q2.0b[Q2.0b$coln == 8,'resp']))

# Averaging by all documents
Q2.0b_8 <- mean_for_multiple_responses(Q2.0b, 8, vulnpop_assess)

###### Number of assessment publications and areas/sectors covered in total
Q2.0b_1 <- count_rows(Q2.0b, 1, n_assess)

###### Number of areas/sectors covered in total
Q2.0b_6 <- count_rows(Q2.0b, 6, areas_assess)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q2.0b_4, Q2.0b_1, Q2.0b_8, Q2.0b_6), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Adaptation targets 2019 ####################################################### 
###### Q3.1a: plans that address climate change adaptation
# Selecting relevant columns
Q3.1a <- cities19 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '3.1a' & coln %in% c(1,2,4,6,7,8)) %>% select(id, org, coln, resp)
Q3.1a$resp <- as.character(Q3.1a$resp)

###### Number of documents
Q3.1a_1 <- count_rows(Q3.1a, 1, n_doc_adaptation)

##### Number of areas covered by plans
Q3.1a_2_categories <- c('Energy',
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
Q3.1a_2 <- count_rows(Q3.1a, 2, n_areas_adaptation_plan, select_from = T, categories = Q3.1a_2_categories)

##### Number of climate hazards factored into plan
#Q3.1a_4_categories <- as.data.frame(table(as.character(Q3.1a[Q3.1a$coln == 4,'resp']))) %>% select(Var1) %>%
#  filter(grepl('>', Var1))
#Q3.1a_4_categories <- as.character(Q3.1a_4_categories$Var1)
#Q3.1a_4 <- count_rows(Q3.1a, 4, n_hazards_plan, select_from = T, categories = Q3.1a_4_categories)

###### Boundary of plan
Q3.1a[Q3.1a$coln == 4,'resp'] <- boundary_question_recode(Q3.1a[Q3.1a$coln == 4,'resp'])
table(as.character(Q3.1a[Q3.1a$coln == 4,'resp']))

# Averaging values of a question about boundaries for all plans
Q3.1a_4 <- mean_for_multiple_responses(Q3.1a, 4, boundary_adaptation_plan)


##### Stage of implementation
# Recoding
Q3.1a[Q3.1a$coln == 6,'resp'] <- stage_question_recode(Q3.1a[Q3.1a$coln == 6,'resp'])
table(as.character(Q3.1a[Q3.1a$coln == 6,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all plans
Q3.1a_6 <- mean_for_multiple_responses(Q3.1a, 6, stage_adaptation_plan)


###### Type of plan
# Recoding to ordinal scale
Q3.1a[Q3.1a$coln == 7,'resp'] <- ifelse(Q3.1a[Q3.1a$coln == 7,'resp'] == 'Standalone', 1,
                                        ifelse(Q3.1a[Q3.1a$coln == 7,'resp'] == 'Integrated mitigation / adaptation', 2,
                                               ifelse(Q3.1a[Q3.1a$coln == 7,'resp'] == 'Addressed in general city master plan', 3,
                                                      ifelse(Q3.1a[Q3.1a$coln == 7,'resp'] == 'Addressed in city sector plan', 4,
                                                             ifelse(Q3.1a[Q3.1a$coln == 7,'resp'] %in% 
                                                                      as.character(str_subset(Q3.1a[Q3.1a$coln == 7,'resp'], "^Other, please specify")), 2,
                                                                    ifelse(Q3.1a[Q3.1a$coln == 7,'resp'] == 'Question not applicable', 9999999, NA))))))
table(as.character(Q3.1a[Q3.1a$coln == 7,'resp'])) # checking the recoding results

# Average type of a plan
Q3.1a_7 <- mean_for_multiple_responses(Q3.1a, 7, adaptation_plan_type)

###### Assessment by the local government

# Recoding
Q3.1a[Q3.1a$coln == 8,'resp'] <- presence_or_absence_recode(Q3.1a[Q3.1a$coln == 8,'resp'])
table(as.character(Q3.1a[Q3.1a$coln == 8,'resp'])) # checking the recoding results

# Averaging values of the question about local government's assessment for all plans
Q3.1a_8 <- mean_for_multiple_responses(Q3.1a, 8, localgov_assess_adaptation_plan)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q3.1a_1, Q3.1a_2, Q3.1a_4, Q3.1a_6, Q3.1a_7,
                                 Q3.1a_8), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Social equity 2019 ####################################################### 
###### Q3.0 Selecting hazards that were addressed by actions 
Q3.0 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.0' & coln %in% c(1,2)) %>% select(-qstn) %>% arrange(org, coln, rown)

Q3.0_2 <- Q3.0 %>% filter(coln %in% 2) %>%
  filter(!resp == "Question not applicable" & !resp == "" & !resp == 'No action currently taken') %>%
  left_join(Q3.0 %>% filter(coln %in% 1), by = c('id', 'org', 'rown')) %>%
  filter(!resp.y == '') %>% select(id, org, resp.y) %>% group_by(id, org) %>%
  mutate(resp = resp.y) %>% select(-resp.y)
Q3.0_2 <- distinct(Q3.0_2)

# Adding an indicator showing that a hazard was addressed
Q3.0_2$selected = 1

# Selecting all hazards reported
Q2.1 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '2.1' & coln %in% c(1,10)) %>% select(-qstn)%>% arrange(org, coln, rown)
Q2.1$resp <- as.character(Q2.1$resp)

# Merging the addressed hazards with all hazards reported
Q2.1_Q3.0_2 <- Q2.1 %>%
  left_join(Q3.0_2, by = c('id', 'org', 'resp')) %>% filter(selected == 1, coln == 1)

# Selecting reports with vulnerable populations and filtering only non-missing values
Q2.1_7 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '2.1' & coln == 10) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  filter(!resp == '')

# Merging the addressed hazards with their respective vulnerable populations
Q2.1_Q3.0_2_Q2.1_7 <- Q2.1_Q3.0_2 %>%
  left_join(Q2.1_7, by = c('id', 'org', 'rown'))

# Defining the number of vulnerable populations addressed
df_vuln_addressed <- Q2.1_Q3.0_2_Q2.1_7 %>% group_by(id, org) %>%
  filter(!is.na(resp.y)) %>% # filtering those who did not report on vulnerable populations for a certain hazard
  summarise(vuln_addressed = n(), .groups = 'drop')

# Defining the number of vulnerable populations in total
df_vuln_total <- Q2.1_7 %>% group_by(id, org) %>%
  summarise(vuln_total = n(), .groups = 'drop')

# Addressed per total
df_vuln_addressed_per_total <- df_vuln_addressed %>%
  left_join(df_vuln_total, by = c('id', 'org')) %>%
  filter(!is.na(vuln_total)) %>% 
  mutate(vuln_addressed_per_total = vuln_addressed/vuln_total) %>%
  select(id, org, vuln_addressed_per_total)
#hist(df_vuln_addressed_per_total$vuln_addressed_per_total)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, df_vuln_addressed_per_total), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

# If a city does not have vulnerable populations, assign it 1
cities19_[is.na(cities19_$vuln_addressed_per_total),'vuln_addressed_per_total'] <- 1

####################################################### Adaptation Actions 2019 ####################################################### 
###### Q2.1 and Q3_1: Hazards in actions per hazards reported
Q3.0 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.0' & coln %in% c(1,2)) %>% select(-qstn) %>% arrange(org, coln, rown)

# Number of hazards in total
Q3.0_1 <- count_rows(Q3.0, 1, n_hazards)

# Number of actions in total
Q3.0_2 <- Q3.0 %>% filter(coln %in% 2) %>%
  filter(!resp == "Question not applicable" & !resp == "" & !resp == 'No action currently taken') %>%
  group_by(id, org) %>% summarise(n_adaptation_actions = n(), .groups = 'drop') 

# Merging 
Q3.0_12 <- Q3.0_1 %>%
  full_join(Q3.0_2, by = c('id', 'org'))

# Number of UNIQUE hazards addressed
Q3.0_12$n_hazards_addressed <- apply(Q3.0_12[, c('n_hazards', 'n_adaptation_actions')], 1, FUN = min)
Q3.0_12$n_hazards_addressed <- ifelse(is.na(Q3.0_12$n_hazards) | is.na(Q3.0_12$n_adaptation_actions) |
                                        Q3.0_12$n_hazards == 9999999 |
                                        Q3.0_12$n_adaptation_actions == 9999999, 0, Q3.0_12$n_hazards_addressed)
Q3.0_12[Q3.0_12 == 9999999 | is.na(Q3.0_12)] <- 0

# Actions per hazard
Q3.0_12$n_actions_per_hazard <- Q3.0_12$n_adaptation_actions/Q3.0_12$n_hazards
Q3.0_12[is.infinite(Q3.0_12$n_actions_per_hazard) | is.nan(Q3.0_12$n_actions_per_hazard), 'n_actions_per_hazard'] <- NA

Q3.0_12 <- Q3.0_12 %>% select(id, org, n_hazards_addressed, n_actions_per_hazard)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q3.0_12), by = c('id', 'org'), type='full')

###### Q3.0
Q3.0 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.0' & coln %in% c(2,4,5,7)) %>% select(id, org, coln, rown, resp) %>% arrange(org, coln, rown)
Q3.0$resp <- as.character(Q3.0$resp)

######  Number of actions
Q3.0_2 <- count_rows(Q3.0, 2, n_adaptation_actions)

######  Status of actions (sum by all actions)
Q3.0[Q3.0$coln == 4, 'resp'] <- ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Scoping', 1,
                                       ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Pre-feasibility study/impact assessment', 2,
                                              ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Pre-implementation', 3,
                                                     ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Implementation', 4,
                                                            ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Implementation complete but not in operation', 5,
                                                                   ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == 'Operation', 6,
                                                                          ifelse(Q3.0[Q3.0$coln == 4, 'resp'] == "Monitoring and reporting", 7,
                                                                                 ifelse(Q3.0[Q3.0$coln == 4, 'resp'] %in% as.character(str_subset(Q3.0[Q3.0$coln == 4, 'resp'], "^Other, please specify")), 3.5, NA))))))))
table(as.character(Q3.0[Q3.0$coln == 4, 'resp']))

# Averaging values of a question about status for all actions
Q3.0_4 <- mean_for_multiple_responses(Q3.0, 4, adaptation_action_status)

######  Number of co-benefits
Q3.0_5 <- count_rows(Q3.0, 5, n_adaptation_cobenefits)

######  Number of Sectors/areas adaptation actions apply to
#Q3.0_7 <- count_rows(Q3.0, 7, n_adaptation_actions_areas)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q3.0_2, Q3.0_4, Q3.0_6), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Adaptation Goals 2019 ####################################################### 
###### Q3.3
Q3.2 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '3.2' & coln == 1) %>% select(id, org, coln, rown, resp) %>%
  arrange(org, coln, rown)

Q3.2$resp <- as.character(Q3.2$resp)

######  Number of goals
Q3.2_1 <- count_rows(Q3.2, 1, n_goals)

######  Number of climate hazards that adaptation goals address
#Q3.2_2 <- count_rows(Q3.2, 2, n_hazards_goals)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q3.2_1), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Inventory 2019 ####################################################### 
###### Q4.0: a city-wide emissions inventory
Q4.0 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '4.0') %>% select(id, org, coln, rown, resp) %>% arrange(org, coln, rown)

# Recoding into ordinal numeric
Q4.0[,'resp'] <- presence_or_absence_recode(Q4.0[,'resp']) 
Q4.0 <- Q4.0 %>% select(id, org, 'inventory' = resp)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q4.0), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Emission Reduction 2019####################################################### 
###### Q5.0: % of target achieved and reduction target in metric tonnes
Q5.0a <- emission_reduction_targets(cities19, '5.0a', 9, 10)
Q5.0b <- emission_reduction_targets(cities19, '5.0b', 7, 8)
Q5.0c <- emission_reduction_targets(cities19, '5.0c', 11, 12)
Q5.0d <- emission_reduction_targets(cities19, '5.0d', 8, 10)

# cities with no target
Q5_no_target <- cities19 %>% select(id, org, qstn, resp) %>%
  filter(qstn == '5.0' & resp == 'No target') %>% 
  mutate(pcnt_target_achieved = 9999999,
         target_metric_tonnes = 9999999) %>% 
  select(-c(qstn, resp)) %>% distinct(.keep_all = T)
#length(unique(Q5_no_target$id))

# Combining all target types and summarising if a city fell into 2 categories
Q5.0abcd <- rbind.data.frame(Q5.0a, Q5.0b, Q5.0c, Q5.0d, Q5_no_target) %>% group_by(id, org) %>%
  summarise_all(list(sum))

##### Q5.4 Mitigation Targets
Q5.4 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '5.4' & coln %in% c(1,4:7,9)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q5.4$resp <- as.character(Q5.4$resp)

# Number of mitigation actions
Q5.4_1 <- count_rows(Q5.4, 1, n_mitigation_actions)

######  Status of actions (sum by all actions)
Q5.4[Q5.4$coln == 4, 'resp'] <- ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Scoping', 1,
                                       ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Pre-feasibility study', 2,
                                              ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Pre-implementation', 3,
                                                     ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Implementation', 4,
                                                            ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Implementation complete', 5,
                                                                   ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == 'Operation', 6,
                                                                          ifelse(Q5.4[Q5.4$coln == 4, 'resp'] == "Monitoring and reporting", 7, NA)))))))
table(as.character(Q5.4[Q5.4$coln == 4, 'resp']))

# Averaging values of a question about status for all actions
Q5.4_4 <- mean_for_multiple_responses(Q5.4, 4, mitigation_action_status)

######  Number of co-benefits
Q5.4_9 <- count_rows(Q5.4, 9, n_mitigation_cobenefits)

###### Estimated emissions reduction: sum for all actions per population size
Q5.4_5 <- sum_for_multiple_responses(Q5.4, 5, estimated_emissions_reduction)

###### Energy savings (MWh): sum for all actions per population size
Q5.4_6 <- sum_for_multiple_responses(Q5.4, 6, energy_savings)

###### Renewable energy production (MWh): sum for all actions per population size
Q5.4_7 <- sum_for_multiple_responses(Q5.4, 7, renewable_energy_production)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q5.0abcd, Q5.4_1, Q5.4_4, Q5.4_5, Q5.4_6,
                                 Q5.4_7, Q5.4_9), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

##### Q5.5a Mitigation Plan
Q5.5a <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '5.5a' & coln %in% c(1,5,7,8)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q5.5a$resp <- as.character(Q5.5a$resp)

######  Number of documents
Q5.5a_1 <- count_rows(Q5.5a, 1, n_doc_mitigation)

##### Number of areas covered by plan
#Q5.5a_5 <- count_rows(Q5.5a, 5, n_areas_mitigation_plan)

###### Boundary of a plan
Q5.5a[Q5.5a$coln == 5,'resp'] <- boundary_question_recode(Q5.5a[Q5.5a$coln == 5,'resp'])
table(as.character(Q5.5a[Q5.5a$coln == 5,'resp']))

# Averaging values of a question about boundaries for all plans
Q5.5a_5 <- mean_for_multiple_responses(Q5.5a, 5, boundary_mitigation_plan)

###### Stage of implementation

# Recoding
Q5.5a[Q5.5a$coln == 7,'resp'] <- stage_question_recode(Q5.5a[Q5.5a$coln == 7,'resp'])
table(as.character(Q5.5a[Q5.5a$coln == 7,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all plans
Q5.5a_7 <- mean_for_multiple_responses(Q5.5a, 7, stage_mitigation_plan)

###### Assessment by the local government

# Recoding
Q5.5a[Q5.5a$coln == 8,'resp'] <- presence_or_absence_recode(Q5.5a[Q5.5a$coln == 8,'resp'])
table(as.character(Q5.5a[Q5.5a$coln == 8,'resp'])) # checking the recoding results

# Averaging values of the question about local government's assessment for all plans
Q5.5a_8 <- mean_for_multiple_responses(Q5.5a, 8, localgov_assess_mitigation_plan)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q5.5a_1, Q5.5a_5, Q5.5a_7, Q5.5a_8), by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Opportunities 2019 ####################################################### 
###### Q6.0: Number of opportunities identified as a result of addressing climate change
Q6.0 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '6.0' & coln == 1) %>% select(-qstn) %>% arrange(org, coln, rown)
Q6.0$resp <- as.character(Q6.0$resp)

# Number of opportunities
Q6.0_1 <- count_rows(Q6.0, 1, n_opportunities)

###### Q6.1a: Collaboration with business
Q6.1a <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '6.1a' & coln == 1) %>% select(-qstn) %>% arrange(org, coln, rown)
Q6.1a$resp <- as.character(Q6.1a$resp)

# Number of collaboration areas
Q6.1a_1 <- count_rows(Q6.1a, 1, n_collab_areas)

# Number of collaboration types
#Q6.1a_2 <- count_rows(Q6.2a, 2, n_collab_types)

###### Q6.2: Collaboration with business
Q6.2 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '6.2' & coln %in% c(1,3,4)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q6.2$resp <- as.character(Q6.2$resp)

# Number of unique project areas
Q6.2_1 <- count_rows(Q6.2, 1, n_project_areas)

###### Stage of implementation

# Recoding
Q6.2[Q6.2$coln == 3,'resp'] <- ifelse(Q6.2[Q6.2$coln == 3,'resp'] == 'Scoping', 1,
                                      ifelse(Q6.2[Q6.2$coln == 3,'resp'] == 'Pre-feasibility study/impact assessment', 2,
                                             ifelse(Q6.2[Q6.2$coln == 3,'resp'] == 'Project feasibility', 3,
                                                    ifelse(Q6.2[Q6.2$coln == 3,'resp'] == 'Project structuring', 4,
                                                           ifelse(Q6.2[Q6.2$coln == 3,'resp'] == 'Transaction preparation', 5,
                                                                  ifelse(Q6.2[Q6.2$coln == 3,'resp'] == 'Implementation', 6,
                                                                         ifelse(Q6.2[Q6.2$coln == 3,'resp'] == "Post-implementation", 7,
                                                                                ifelse(Q6.2[Q6.2$coln == 3, 'resp'] == 'Question not applicable', 9999999, NA))))))))
table(as.character(Q6.2[Q6.2$coln == 3,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all projects
Q6.2_3 <- mean_for_multiple_responses(Q6.2, 3, stage_project)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q6.0_1, Q6.1a_1, Q6.2_1, Q6.2_3),
                            by = c('id', 'org'), type='full')
#length(unique(Q6.5_3$id))
####################################################### Renewable Energy Target 2019 ####################################################### 
###### Q8.0a 
Q8.0a <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '8.0a' & coln %in% c(8,9)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q8.0a$resp <- as.character(Q8.0a$resp)
Q8.0a[Q8.0a$resp == 'Question not applicable', 'resp'] <- 9999999

# Average percentage of renewable energy / electricity of total energy or electricity in target year
Q8.0a_8 <- mean_for_multiple_responses(Q8.0a, 8, percentage_renewable_energy)

# Average percentage of targets achieved
Q8.0a_9 <- mean_for_multiple_responses(Q8.0a, 9, renewable_energy_achieved)

###### Q8.2 the source mix of electricity consumed
Q8.2 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '8.2' & coln %in% 5:9) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  group_by(id, org) %>%
  summarise(renewable_energy_source = ifelse(all(is.na(as.numeric(as.character(resp)))),
                                             NA, sum(as.numeric(as.character(resp)), na.rm = T)), .groups = 'drop')

####### Q8.6a city’s energy efficiency targets
Q8.6a <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '8.6a' & coln %in% c(7,8)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q8.6a$resp <- as.character(Q8.6a$resp)
Q8.6a[Q8.6a$resp == 'Question not applicable', 'resp'] <- 9999999

# Average percentage of energy efficiency improvement in target year compared to base year levels
Q8.6a_7 <- mean_for_multiple_responses(Q8.6a, 7, percentage_energy_efficiency_improvement)

# Average percentage of target achieved
Q8.6a_8 <- mean_for_multiple_responses(Q8.6a, 8, energy_efficiency_achieved)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q8.0a_8, Q8.0a_9, Q8.2, Q8.6a_7, Q8.6a_8),
                            by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Transport 2019 ####################################################### 
###### Q10.1 the mode share of each transport mode
Q10.1 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '10.1' & coln %in% c(5,6)) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  group_by(id, org) %>% filter(!resp == "Question not applicable") %>%
  summarise(percentage_walking_cycling = ifelse(all(is.na(as.numeric(as.character(resp)))),
                                                NA, sum(as.numeric(as.character(resp)), na.rm = T)), .groups = 'drop')
# Adding cities with 'not applicable" responses
Q10.1 <- rbind.data.frame(Q10.1, cities19 %>% filter(qstn == '10.1' & coln %in% c(5,6)) %>%
                            filter(resp == "Question not applicable") %>%
                            select(id, org) %>%
                            mutate(percentage_walking_cycling = 9999999)) %>% distinct(.keep_all = T)

###### Q10.4 Total number of vehicle types for private cars (Electric + Hybrid + Plug in hybrid + Hydrogen)
Q10.5 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '10.5' & coln == 1 & rown %in% 2:5) %>% select(-qstn) %>% arrange(org, coln, rown)%>%
  group_by(id, org) %>% filter(!resp == "Question not applicable") %>%
  summarise(eco_private_cars = ifelse(all(is.na(as.numeric(as.character(resp)))),
                                      NA, sum(as.numeric(as.character(resp)), na.rm = T)), .groups = 'drop')
# Adding cities with 'not applicable" responses
Q10.5 <- rbind.data.frame(Q10.5, cities19 %>% filter(qstn == '10.5' & coln == 1 & rown %in% 2:5) %>%
                            filter(resp == "Question not applicable") %>%
                            select(id, org) %>%
                            mutate(eco_private_cars = 9999999)) %>% distinct(.keep_all = T)

###### Q10.7a: zero emissions zone presence
Q10.7 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '10.7') %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  mutate(zero_emission_zone_presence = ifelse(resp == 'Yes', 1,
                                              ifelse(resp == 'No' | resp == 'Do not know', 0, NA))) %>%
  select(-c(coln, rown, resp))

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q10.1, Q10.5, Q10.7),
                            by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Food 2019 ####################################################### 
###### Q12.4: presence of policies relating to food consumption
Q12.4 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '12.4' & coln == 1) %>% select(-qstn) %>% arrange(org, coln, rown)%>%
  group_by(id, org) %>% mutate(food_policies_presence = ifelse(resp == 'Yes', 1, 
                                                               ifelse(resp == 'No' | resp == 'Do not know', 0, NA)))%>%
  select(-c(coln, rown, resp))


# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q12.4),
                            by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Waste 2019 ####################################################### 
###### Q13.0: Amount of solid waste generated (tonnes/year) per population size
Q13.0 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '13.0' & coln == 1 & rown == 1) %>% select(-qstn) %>% arrange(org, coln, rown) %>%
  mutate(waste_amount = as.numeric(as.character(resp))) %>%
  select(-c(coln, rown, resp))

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q13.0),
                            by = c('id', 'org'), type='full')
#length(unique(cities19_$id))

####################################################### Water Security 2019 ####################################################### 
###### Q14.3
Q14.4 <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '14.4' & coln %in% 2:3) %>% select(-qstn) %>% arrange(org, coln, rown)
Q14.4$resp <- as.character(Q14.4$resp)

# Number of unique adaptation actions
Q14.4_2 <- count_rows(Q14.4, 2, n_water_adaptation_actions)

# Status
Q14.4[Q14.4$coln == 3, 'resp'] <- ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Scoping', 1,
                                         ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Pre-feasibility study/impact assessment', 2,
                                                ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Pre-implementation', 3,
                                                       ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Implementation', 4,
                                                              ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Implementation complete but not in operation', 5,
                                                                     ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Operation', 6,
                                                                            ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == "Monitoring and reporting", 7,
                                                                                   ifelse(Q14.4[Q14.4$coln == 3, 'resp'] == 'Question not applicable', 9999999, NA))))))))
table(as.character(Q14.4[Q14.4$coln == 3, 'resp']))

# Averaging values for all water adaptation actions
Q14.4_3 <- mean_for_multiple_responses(Q14.4, 3, water_adaptation_action_status)
length(unique(Q14.4_3$id))

###### Q14.5a: 
Q14.5a <- cities19 %>% select(id, org, qstn, coln, rown, resp) %>% 
  filter(qstn == '14.5a' & coln %in% c(1,5)) %>% select(-qstn) %>% arrange(org, coln, rown)
Q14.5a$resp <- as.character(Q14.5a$resp)

# Number of documents
Q14.5a_1 <- count_rows(Q14.5a, 1, n_water_management)

# Stage of implementation
Q14.5a[Q14.5a$coln == 5, 'resp'] <- stage_question_recode(Q14.5a[Q14.5a$coln == 5, 'resp'])
table(as.character(Q14.5a[Q14.5a$coln == 5,'resp'])) # checking the recoding results

# Averaging values of the question about a stage for all plans
Q14.5a_5 <- mean_for_multiple_responses(Q14.5a, 5, stage_water_management_plan)

# Merging with the selected variables so far
cities19_ <- plyr::join_all(list(cities19_, Q14.4_2, Q14.4_3, Q14.5a_1, Q14.5a_5),
                            by = c('id', 'org'), type='full')
#length(unique(cities19_$id))





####################################################### KPIs 2020 2019 #######################################################
# Replacing 9999999 with 0
cities19_[cities19_ == 9999999] <- 0

# Filtering if popsize is missing (meaning these cities are not present in 2020 dataset)
cities19_ <- cities19_ %>% filter(!is.na(popsize))

# Norming by population size a range of items
cities19_$target_metric_tonnes <- cities19_$target_metric_tonnes/cities19_$popsize
cities19_$estimated_emissions_reduction <- cities19_$estimated_emissions_reduction/cities19_$popsize
cities19_$energy_savings <- cities19_$energy_savings/cities19_$popsize
cities19_$renewable_energy_production <- cities19_$renewable_energy_production/cities19_$popsize
cities19_$waste_amount <- cities19_$waste_amount/cities19_$popsize

# Replacing NA with 0s
cities19_[is.na(cities19_)] <- 0
#summary(cities19_items_only_2)

# Selecting only cities present in both 2020 and 2019
cities19_items_only <- cities19_ %>% filter(id %in% cities20_items_only_2$id) %>%
  select(-c(qstn, coln, popsize, landarea)) %>%
  select(-c(n_goals, # filtering items with low response rates in 2020
            n_mitigation_cobenefits,
            stage_project,
            eco_private_cars))
cities20_items_only_2 <- cities20_items_only_2 %>% filter(id %in% cities19_items_only$id)

# Adding Year ID
cities19_items_only$year <- 2019
cities20_items_only_2$year <- 2020

# Adding contextual variables to 2019, the same as in 2020
cities19_items_only <- cities19_items_only %>% arrange(id)
cities20_items_only_2 <- cities20_items_only_2 %>% arrange(id)
cities19_items_only[, c('gdp_per_capita',
                        "hazards_intensity",                       
                        "population",                            
                        "mean_temp",                               
                        "sum_precip",                              
                        "no2_per_population")] <- cities20_items_only_2[, c('gdp_per_capita',
                                                                            "hazards_intensity",                       
                                                                            "population",                            
                                                                            "mean_temp",                               
                                                                            "sum_precip",                              
                                                                            "no2_per_population")]
# Row bind all data to obtain a full variance for further scaling
cities19_20 <- plyr::rbind.fill(cities20_items_only_2, cities19_items_only)

# Scaling all items based on empirical data: max = max for 2 years, min = min for 2 years under focus
# This is needed to ensure unification of a scale across years

# 2019
cities19_norm <- cities19_20[cities19_20$year == 2019,]
for (i in colnames(cities19_norm)[3:56]){
  for (j in 1:nrow(cities19_norm)){
    cities19_norm[j,i] <- (cities19_norm[j,i] - min(cities19_20[,i], na.rm = T)) /
      (max(cities19_20[,i], na.rm = T) - min(cities19_20[,i], na.rm = T))
  }
}

# 2020
cities20_norm <- cities19_20[cities19_20$year == 2020,]
for (i in colnames(cities20_norm)[3:56]){
  for (j in 1:nrow(cities20_norm)){
    cities20_norm[j,i] <- (cities20_norm[j,i] - min(cities19_20[,i], na.rm = T)) /
      (max(cities19_20[,i], na.rm = T) - min(cities19_20[,i], na.rm = T))
  }
}

#summary(cities19_norm)
#summary(cities20_norm)

# Taking care of reverse scales
cities19_norm$waste_amount <- 1 - cities19_norm$waste_amount
cities19_norm$no2_per_population <- 1 - cities19_norm$no2_per_population
cities20_norm$waste_amount <- 1 - cities20_norm$waste_amount
cities20_norm$no2_per_population <- 1 - cities20_norm$no2_per_population

summary(cities19_norm$waste_amount)
summary(cities20_norm$waste_amount)

# Waste does not seem to have a good variation -> drop it
cities19_norm$waste_amount <- NULL
cities20_norm$waste_amount <- NULL

############################################### KPI 2020

# Summing items by subgroups
cities20_norm$Risk_Assessment <- rowMeans(cities20_norm[,c('boundary_assess',
                                                           'n_assess',
                                                           'areas_assess')])
hist(cities20_norm$Risk_Assessment, breaks = 50)
cities20_norm$Adaptation_Plan <- rowMeans(cities20_norm[,c('n_doc_adaptation',
                                                           'n_areas_adaptation_plan',
                                                           'n_hazards_plan',
                                                           'boundary_adaptation_plan',
                                                           'stage_adaptation_plan',
                                                           'adaptation_plan_type',
                                                           'localgov_assess_adaptation_plan')])
hist(cities20_norm$Adaptation_Plan, breaks = 50)
cities20_norm$Adaptation_Actions <- rowMeans(cities20_norm[,c('n_hazards_addressed',
                                                              'n_actions_per_hazard',
                                                              'n_adaptation_actions',
                                                              'adaptation_action_status',
                                                              'n_adaptation_cobenefits')])
hist(cities20_norm$Adaptation_Actions, breaks = 50)
cities20_norm$Mitigation_Actions <- rowMeans(cities20_norm[,c('inventory',
                                                              'target_metric_tonnes',
                                                              'pcnt_target_achieved',
                                                              'n_mitigation_actions',
                                                              'mitigation_action_status',
                                                              'estimated_emissions_reduction',
                                                              'energy_savings',
                                                              'renewable_energy_production',
                                                              'no2_per_population')])
hist(cities20_norm$Mitigation_Actions, breaks = 50)
cities20_norm$Mitigation_Plan <- rowMeans(cities20_norm[,c('n_doc_mitigation',
                                                           'n_areas_mitigation_plan',
                                                           'boundary_mitigation_plan',
                                                           'stage_mitigation_plan',
                                                           'localgov_assess_mitigation_plan')])
hist(cities20_norm$Mitigation_Plan, breaks = 50)
cities20_norm$Opportunities <- rowMeans(cities20_norm[,c('n_opportunities',
                                                         'n_collab_areas',
                                                         'n_collab_types',
                                                         'n_project_areas')])
hist(cities20_norm$Opportunities, breaks = 50)
cities20_norm$Renewable_Energy_Target <- rowMeans(cities20_norm[,c('percentage_renewable_energy',
                                                                   'renewable_energy_achieved',
                                                                   'renewable_energy_source',
                                                                   'percentage_energy_efficiency_improvement',
                                                                   'energy_efficiency_achieved')])
hist(cities20_norm$Renewable_Energy_Target, breaks = 50)
cities20_norm$Additional_Measures <- rowMeans(cities20_norm[,c('percentage_walking_cycling',
                                                               'zero_emission_zone_presence',
                                                               'food_policies_presence')])
hist(cities20_norm$Additional_Measures, breaks = 50)
cities20_norm$Water <- rowMeans(cities20_norm[,c('n_water_adaptation_actions',
                                                 'water_adaptation_action_status',
                                                 'n_water_management',
                                                 'stage_water_management_plan')])
hist(cities20_norm$Water, breaks = 50)
cities20_norm$Social_Equity <- rowMeans(cities20_norm[,c('vulnpop_assess',
                                                         'vuln_addressed_per_total')])
hist(cities20_norm$Social_Equity, breaks = 50)

# TOTAL KPI
cities20_KPIs <- cities20_norm[, c('id', 'org',
                                   "Risk_Assessment" ,                        
                                   "Adaptation_Plan" ,                        
                                   "Adaptation_Actions" ,                     
                                   "Mitigation_Actions" ,                     
                                   "Mitigation_Plan",                         
                                   "Opportunities",                           
                                   "Renewable_Energy_Target" ,                
                                   "Additional_Measures",                     
                                   "Water",                                   
                                   "Social_Equity",
                                   'hazards_intensity',
                                   'population', 
                                   'mean_temp',
                                   'sum_precip',
                                   'gdp_per_capita')]

# Computing the final index
cities20_KPIs$KPI_total <- 100*rowMeans(cities20_KPIs[,3:(ncol(cities20_KPIs)-5)])
hist(cities20_KPIs$KPI_total, breaks = 20)
plot(density(cities20_KPIs$KPI_total))
# summary(cities20_KPIs)

############################################### KPI 2019

# Summing items by subgroups
cities19_norm$Risk_Assessment <- rowMeans(cities19_norm[,c('boundary_assess',
                                                           'n_assess',
                                                           'areas_assess')])
hist(cities19_norm$Risk_Assessment, breaks = 50)
cities19_norm$Adaptation_Plan <- rowMeans(cities19_norm[,c('n_doc_adaptation',
                                                           'n_areas_adaptation_plan',
                                                           'boundary_adaptation_plan',
                                                           'stage_adaptation_plan',
                                                           'adaptation_plan_type',
                                                           'localgov_assess_adaptation_plan')])
hist(cities19_norm$Adaptation_Plan, breaks = 50)
cities19_norm$Adaptation_Actions <- rowMeans(cities19_norm[,c('n_hazards_addressed',
                                                              'n_actions_per_hazard',
                                                              'adaptation_action_status',
                                                              'n_adaptation_cobenefits')])
hist(cities19_norm$Adaptation_Actions, breaks = 50)
cities19_norm$Mitigation_Actions <- rowMeans(cities19_norm[,c('inventory',
                                                              'target_metric_tonnes',
                                                              'pcnt_target_achieved',
                                                              'n_mitigation_actions',
                                                              'mitigation_action_status',
                                                              'estimated_emissions_reduction',
                                                              'energy_savings',
                                                              'renewable_energy_production',
                                                              'no2_per_population')])
hist(cities19_norm$Mitigation_Actions, breaks = 50)
cities19_norm$Mitigation_Plan <- rowMeans(cities19_norm[,c('n_doc_mitigation',
                                                           'boundary_mitigation_plan',
                                                           'stage_mitigation_plan',
                                                           'localgov_assess_mitigation_plan')])
hist(cities19_norm$Mitigation_Plan, breaks = 50)
cities19_norm$Opportunities <- rowMeans(cities19_norm[,c('n_opportunities',
                                                         'n_collab_areas',
                                                         'n_project_areas')])
hist(cities19_norm$Opportunities, breaks = 50)
cities19_norm$Renewable_Energy_Target <- rowMeans(cities19_norm[,c('percentage_renewable_energy',
                                                                   'renewable_energy_achieved',
                                                                   'renewable_energy_source',
                                                                   'percentage_energy_efficiency_improvement',
                                                                   'energy_efficiency_achieved')])
hist(cities19_norm$Renewable_Energy_Target, breaks = 50)
cities19_norm$Additional_Measures <- rowMeans(cities19_norm[,c('percentage_walking_cycling',
                                                               'zero_emission_zone_presence',
                                                               'food_policies_presence')])
hist(cities19_norm$Additional_Measures, breaks = 50)
cities19_norm$Water <- rowMeans(cities19_norm[,c('n_water_adaptation_actions',
                                                 'water_adaptation_action_status',
                                                 'n_water_management',
                                                 'stage_water_management_plan')])
hist(cities19_norm$Water, breaks = 50)
cities19_norm$Social_Equity <- rowMeans(cities19_norm[,c('vulnpop_assess',
                                                         'vuln_addressed_per_total')])
hist(cities19_norm$Social_Equity, breaks = 50)

# TOTAL KPI
cities19_KPIs <- cities19_norm[, c('id', 'org',
                                   "Risk_Assessment" ,                        
                                   "Adaptation_Plan" ,                        
                                   "Adaptation_Actions" ,                     
                                   "Mitigation_Actions" ,                     
                                   "Mitigation_Plan",                         
                                   "Opportunities",                           
                                   "Renewable_Energy_Target" ,                
                                   "Additional_Measures",                     
                                   "Water",                                   
                                   "Social_Equity",
                                   'hazards_intensity',
                                   'population', 
                                   'mean_temp',
                                   'sum_precip',
                                   'gdp_per_capita')]

# Computing the final index
cities19_KPIs$KPI_total <- 100*rowMeans(cities19_KPIs[,3:(ncol(cities19_KPIs)-5)])
hist(cities19_KPIs$KPI_total, breaks = 20)
plot(density(cities19_KPIs$KPI_total))
summary(cities19_KPIs)

# Recode to UTF-8 2019
cities19_KPIs_org <- cities19_KPIs$org
Encoding(cities19_KPIs_org) <- "UTF-8"
cities19_KPIs$org <- cities19_KPIs_org

####################################################### Reliability #######################################################
###### Correlation

# 2020
cor.test(cities20_KPIs$KPI_total, cities20_KPIs$gdp_per_capita)
KPI_top <- cities20_KPIs %>% arrange(desc(KPI_total))

plot(cities20_KPIs$KPI_total, cities20_KPIs$gdp_per_capita)
cities20_KPIs <- cities20_KPIs %>% filter(!id == 31090)

# 2020
cor.test(cities19_KPIs$KPI_total, cities19_KPIs$gdp_per_capita)
KPI_top <- cities19_KPIs %>% arrange(desc(KPI_total))

plot(cities19_KPIs$KPI_total, cities19_KPIs$gdp_per_capital)
cities19_KPIs <- cities19_KPIs %>% filter(!id == 31090)

###### Densities

# Combining data for 2019 and 2020
# Joint dataset
cities20_KPIs_year <- cbind.data.frame(year = 2020, cities20_KPIs)
cities19_KPIs_year <- cbind.data.frame(year = 2019, cities19_KPIs)
cities19_20_KPIs_year <- rbind.data.frame(cities20_KPIs_year,
                                          cities19_KPIs_year) %>% arrange(year)
# Graphing
cities19_20_KPIs_year %>%
  ggplot(aes(x = KPI_total, fill = factor(year))) +
  geom_histogram(bins = 10) + 
  labs(x = "Total KPI Scores",
       title = "The Probability Density Functions, 2019 and 2020")

##### Difference in total KPI
cities19_20_KPIs <- cities20_KPIs[,c('org', 'KPI_total')] %>%
  left_join(cities19_KPIs[,c('org', 'KPI_total')], by = 'org') %>%
  mutate(diff_KPI = KPI_total.x - KPI_total.y) %>%
  arrange(desc(diff_KPI))
colnames(cities19_20_KPIs) <- c('City', "Total KPI 2020", 'Total KPI 2019', 'Difference')

# Selecting top 20
cities19_20_KPIs_top20 <- cities19_20_KPIs %>% slice(1:20)
rownames(cities19_20_KPIs_top20) <- cities19_20_KPIs_top20$City
cities19_20_KPIs_top20 <- cities19_20_KPIs_top20 %>% select(-City)
cities19_20_KPIs_top20 <- as.matrix(cities19_20_KPIs_top20)
heatmap(cities19_20_KPIs_top20)

# Heatmap
####################################################### Financial Data #######################################################

##### Currency unification
df_currency <- cities20 %>%
  filter(qstn == "0.4", resp!="") %>%
  select(id, resp) %>% rename(currency = resp) %>%
  mutate(currency = substr(currency, 1, 3)) 
currency_rate_usd <- getQuote(paste0(df_currency$currency, rep("USD", nrow(df_currency)), "=X"))
currency_rate_usd <- currency_rate_usd %>% select(Last) %>%
  mutate(currency = substr(rownames(currency_rate_usd), 1, 3)) %>%
  rename(curr_rate = Last)
df_currency <- df_currency %>% left_join(currency_rate_usd) %>% select(-c(currency))

###### Q3.0_11 total cost of adaptation actions
Q3.0_11 <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '3.0' & coln == 11) %>% select(id, org, resp) %>%
  filter(!resp %in% c('', '0')) %>% group_by(id, org) %>%
  summarise(resp = sum(as.numeric(as.character(resp))), .groups = 'drop') 

# Changing currency
Q3.0_11 <- Q3.0_11 %>% left_join(df_currency, by = 'id') %>% 
  mutate(adaptation_cost_usd = log(resp*curr_rate)) %>% filter(!adaptation_cost_usd < 0)
hist(Q3.0_11$adaptation_cost_usd, breaks=50)

###### Q5.4_12: total cost of mitigation actions
Q5.4_12 <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '5.4' & coln == 12) %>% select(id, org, coln, resp) %>%
  filter(!resp %in% c('', '0')) %>% group_by(id, org) %>%
  summarise(resp = sum(as.numeric(as.character(resp))), .groups = 'drop')

# Changing currency
Q5.4_12 <- Q5.4_12 %>% left_join(df_currency, by = 'id') %>% 
  mutate(mitigation_cost_usd = log(resp*curr_rate))  %>% filter(!mitigation_cost_usd < 0)
hist(Q5.4_12$mitigation_cost_usd, breaks=50)

# Q6.5_8: total cost of projects
Q6.5_8 <- cities20 %>% select(id, org, qstn, coln, resp) %>% 
  filter(qstn == '6.5' & coln == 8) %>% select(id, org, coln, resp) %>%
  filter(!resp %in% c('', '0')) %>% group_by(id, org) %>%
  summarise(resp = sum(as.numeric(as.character(resp))), .groups = 'drop')

# Changing currency
Q6.5_8 <- Q6.5_8 %>% left_join(df_currency, by = 'id') %>%
  mutate(project_cost_usd = log(resp*curr_rate))  %>% filter(!project_cost_usd < 0)
hist(Q6.5_8$project_cost_usd, breaks=50)





