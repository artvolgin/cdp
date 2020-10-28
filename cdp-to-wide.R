
# Libraries
library(foreign)
library(dplyr)

# Corporations: Climate Change Data
common_dir <- paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/')

# Set wd
out_names <- list(c('Wide_Climate_2018', 'Wide_Climate_2019', 'Wide_Climate_2020'),
               c('Wide_Water_2018', 'Wide_Water_2019', 'Wide_Water_2020'),
               c('Wide_Cities_2018', 'Wide_Cities_2019', 'Wide_Cities_2020'))

paths <- c(paste0(common_dir,'Corporations/Corporations Responses/Climate Change/'),
           paste0(common_dir,'Corporations/Corporations Responses/Water Security/'),
           paste0(common_dir,'Cities/Cities Responses/'))

for (i in 1:length(paths)){
  file_names <- list.files(path = paths[i], pattern = 'Dataset.csv')
  
  for (j in 1:length(file_names)){
    df <- read.csv(paste0(paths[i], file_names[j]))
    
    if (i == 3) {# Make names unified
    colnames(df) <- tolower(colnames(df)) # lower case
    colnames(df)[which(colnames(df) %in% c('organization', 'question.number',
                                           'response.answer', 'column.number', 'row.number', 'row.name'))] <- 
      c('organization',  'question_number', 'response_value', 'column_number', 'row_number', 'row_name')
    }
    
    # Unique ID
    df[, 'cell'] <- paste0(df[, 'question_number'], "_", df[, 'column_number'], "_",
                           ifelse(is.na(df[, 'row_name']), df[, 'row_name'], df[, 'row_number']))
    
    # Merging cells for a single response (where cell id is identical)
    # It can be identical if a person adds rows for responses manually
    df_temp <- df %>%
      group_by(organization, cell) %>%
      summarise_at(vars(response_value), funs(paste(., collapse = '  $BREAK$  ')))
    
    # Removing response cells with identicall id from the main df and joining it with
    # temp where responses for a singe question are merged
    df_ <- df %>% group_by(organization) %>% 
      filter(!duplicated(cell)) %>% select(-response_value)
    df_corrected <- df_temp %>%
      left_join(df_, by = c('organization', 'cell')) %>% select(organization, cell, response_value)
    
    # Transforming to long format
    df_fin <- tidyr::pivot_wider(df_corrected, id_cols = organization,
                                 names_from = cell, values_from = response_value)
    # Saving
    write.csv(df_fin, paste0(common_dir, 'Data Wide/', out_names[[i]][j], '.csv'))
    
    df <- NULL
    df_temp <- NULL
    df_ <- NULL
    df_corrected <- NULL
    df_fin <- NULL
    print(j)
  }
  
}

