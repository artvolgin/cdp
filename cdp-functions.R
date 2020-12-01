
# Functions
###### Boundary of assessment
boundary_question_recode <- function(df){
  boundary_vec <- as.data.frame(table(as.character(df)))
  boundary_vec <- as.character(boundary_vec$Var1)
  # replacing categrical values from a vector by ordinal values
  df <- ifelse(df == boundary_vec[str_detect(boundary_vec, "^Partial")], 2,
        ifelse(df == boundary_vec[str_detect(boundary_vec, "^Larger")], 4,
        ifelse(df == boundary_vec[str_detect(boundary_vec, "^Smaller")], 1,
        ifelse(df == boundary_vec[str_detect(boundary_vec, "^Same")], 3, 
        ifelse(df == 'Question not applicable', 9999999, NA)))))
} # A function for recoding categorical values

count_rows <- function(subset, n, resp_name, select_from = F, categories, year = 2020){
  # Counting distinct rows by cities
  if (select_from == T){
    result <- subset %>% filter(coln %in% n) %>%
      filter(!resp == "Question not applicable" & !resp == "" 
             & !resp == 'No opportunities identified' & !resp == 'No relevant projects') %>% 
      filter(resp %in% {{categories}}) %>%
      group_by(id, org) %>% summarise({{resp_name}} := n_distinct(resp), .groups = 'drop')
  }
  else {
    result <- subset %>% filter(coln %in% n) %>%
      filter(!resp == "Question not applicable" & !resp == "" 
             & !resp == 'No opportunities identified'  & !resp == 'No relevant projects') %>%
      group_by(id, org) %>% summarise({{resp_name}} := n_distinct(resp), .groups = 'drop')    
  }
  
  # Adding cities with 'not applicable" responses
  if (year == 2020){
      result <- rbind.data.frame(result, subset %>% filter(coln %in% n) %>%
                               filter(resp == "Question not applicable" 
                               | (resp == 'No opportunities identified' & !resp %in% result$org)
                               | resp == 'No relevant projects') %>%
                               select(id, org) %>%
                    mutate({{resp_name}} := 9999999)) %>% distinct(.keep_all = TRUE)
  }
  else{
    # 2019
    na_set <- subset %>% filter(coln == n) %>% filter(is.na(resp)|resp == "") %>% select(org)
    result <- rbind.data.frame(result, full_cities19 %>% filter(!org %in% unique(result$org) & !org %in% na_set$org) %>%
                                 select(id, org) %>%
                                 mutate({{resp_name}} := 9999999)) %>%
      distinct(.keep_all = T)
  }
  
  # Filtering if a city appeared 2 times because of valid and missing values in the whole response for a question
  result <- result %>%
    filter(!(org %in% as.character(result[duplicated(result$org), 'org']$org) & {{resp_name}} == 9999999))
  
  return(result)
}

# A function to sum values for ordinal multiple response questions
sum_for_multiple_responses <- function(subset, n, resp_name, year = 2020){
  result <- subset %>% group_by(id, org) %>% filter(coln == n) %>% 
    filter(!resp == 9999999 & !resp == "") %>%
    summarise({{resp_name}} := sum(as.numeric(as.character(resp)), na.rm = T), .groups = 'drop')
  
  # Adding cities with 'not applicable" responses
  # 2020
  if (year == 2020){
      result <- rbind.data.frame(result, subset %>% filter(coln %in% n) %>%
                               filter(resp == 9999999) %>%
                               select(id, org) %>%
                               mutate({{resp_name}} := 9999999)) %>%
    distinct(.keep_all = T)
  }
  else{
    # 2019
    na_set <- subset %>% filter(coln == n) %>% filter(is.na(resp)|resp == "") %>% select(org)
    result <- rbind.data.frame(result, full_cities19 %>% filter(!org %in% unique(result$org) & !org %in% na_set$org) %>%
                                 select(id, org) %>%
                                 mutate({{resp_name}} := 9999999)) %>%
      distinct(.keep_all = T)
  }
  # Filtering if a city appeared 2 times because of valid and missing values in the whole response for a question
  result <- result %>%
    filter(!(org %in% as.character(result[duplicated(result$org), 'org']$org) & {{resp_name}} == 9999999))

  return(result)
  
}

# A function to average values for ordinal multiple response questions
mean_for_multiple_responses <- function(subset, n, resp_name, year = 2020){
  result <- subset %>% group_by(id, org) %>% filter(coln == n) %>% 
    filter(!resp == 9999999 & !resp == "") %>%
    summarise({{resp_name}} := mean(as.numeric(as.character(resp)), na.rm = T), .groups = 'drop')
  
  # Adding cities with 'not applicable" responses
  # 2020
  if (year == 2020){
    result <- rbind.data.frame(result, subset %>% filter(coln %in% n) %>%
                                 filter(resp == 9999999) %>%
                                 select(id, org) %>%
                                 mutate({{resp_name}} := 9999999)) %>%
      distinct(.keep_all = T)
  }
  else{
    # 2019
    na_set <- subset %>% filter(coln == n) %>% filter(is.na(resp)|resp == "") %>% select(org)
    result <- rbind.data.frame(result, full_cities19 %>% filter(!org %in% unique(result$org) & !org %in% na_set$org) %>%
                                 select(id, org) %>%
                                 mutate({{resp_name}} := 9999999)) %>%
      distinct(.keep_all = T)
  }
  # Filtering if a city appeared 2 times because of valid and missing values in the whole response for a question
  result <- result %>%
    filter(!(org %in% as.character(result[duplicated(result$org), 'org']$org) & {{resp_name}} == 9999999))
  
  return(result)
  
}

# A function to take maximum in multiple response questions
max_for_multiple_responses <- function(subset, n, resp_name, year = 2020){
  result <- subset %>% group_by(id, org) %>% filter(coln == n) %>% 
    filter(!resp == 9999999 & !resp == "") %>%
    summarise({{resp_name}} := max(as.numeric(as.character(resp)), na.rm = T), .groups = 'drop')
  
  # Adding cities with 'not applicable" responses
  # 2020
  if (year == 2020){
    result <- rbind.data.frame(result, subset %>% filter(coln %in% n) %>%
                                 filter(resp == 9999999) %>%
                                 select(id, org) %>%
                                 mutate({{resp_name}} := 9999999)) %>%
      distinct(.keep_all = T)
  }
  else{
    # 2019
    na_set <- subset %>% filter(coln == n) %>% filter(is.na(resp)|resp == "") %>% select(org)
    result <- rbind.data.frame(result, full_cities19 %>% filter(!org %in% unique(result$org) & !org %in% na_set$org) %>%
                                 select(id, org) %>%
                                 mutate({{resp_name}} := 9999999)) %>%
      distinct(.keep_all = T)
  }
  # Filtering if a city appeared 2 times because of valid and missing values in the whole response for a question
  result <- result %>%
    filter(!(org %in% as.character(result[duplicated(result$org), 'org']$org) & {{resp_name}} == 9999999))
  
  return(result)
}

stage_question_recode <- function(df){
  # replacing categrical values from a vector by ordinal values
  df <- ifelse(df == 'Plan in development', 1,
        ifelse(df == 'Plan developed but not implemented', 2,
        ifelse(df == 'Plan in implementation', 3,
        ifelse(df == 'Implementation complete', 4,
        ifelse(df == 'Monitoring and evaluation in progress' | df == 'Measurement in progress', 5,
        ifelse(df == 'Plan update in progress', 6,
        ifelse(df %in% as.character(str_subset(df, "^Other, please specify")), 3,
        ifelse(df == 'Question not applicable', 9999999, NA))))))))
} # A function to recode stage-related questions

# A function to recode presence-absence-type categorial questions into ordinal
presence_or_absence_recode <- function(df){
  df <- ifelse(df == 'Yes', 3,
        ifelse(df == 'In Progress' | df == 'In progress', 2,
        ifelse(df == 'Intending to undertake in the next 2 years', 1,
        ifelse(df == 'Not intending to undertake', 0,
        ifelse(df == "Donâ€™t know", 0, 
        ifelse(df == 'Question not applicable', 9999999, NA))))))
}

# A fucntion to extract emission targets
emission_reduction_targets <- function(df, qstn, coln_pcnt_achieved, coln_metric_tonnes){
  df_fin <- df %>% select(id, org, qstn, coln, rown, resp) %>% 
    filter(qstn == {{qstn}} & coln %in% c(coln_pcnt_achieved, coln_metric_tonnes)) %>% 
    select(id, org, coln, rown, resp) %>%
    arrange(org, coln, rown) %>% filter(!resp %in% c('Question not applicable', '')) %>%
    mutate(resp = as.numeric(as.character(resp))) %>% group_by(id, org, coln) %>%
    summarise(resp = mean(resp), .groups = 'drop') %>% 
    pivot_wider(id_cols = c(id, org), names_from = coln, names_prefix = 'var',
                values_from = resp, values_fill = list(nprog = NA)) %>%
    rename(pcnt_target_achieved = paste0('var', {{coln_pcnt_achieved}}),
           target_metric_tonnes = paste0('var', {{coln_metric_tonnes}}))
  
  return(df_fin)
}

# Min-Max normalization function
min_max <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}







































