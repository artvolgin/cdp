

# Basic
library(ggplot2)
library(ggrepel)
library(ggfortify)
library(reshape2)
library(stringr)
library(rio)
library(gridExtra)
library(tidyr)
library(dplyr)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/CDP/data/Data Wide'))

# Load the data
df <- import("Wide_Climate_2020.csv")

# Select only columns with text
text_columns <- c("C2.2a_2_1", "C2.2a_2_2", "C2.2a_2_3", "C2.2a_2_4", "C2.2a_2_5", "C2.2a_2_6",
                  "C2.2a_2_7", "C2.2a_2_8", "C2.4a_14_NA", "C2.4a_16_NA", "C2.4a_17_NA", "C2.4a_6_NA",
                  "C4.1a_15_NA", "C4.1b_18_NA", "C4.2b_14_NA", "C4.2b_16_NA", "C4.3b_9_NA", "C4.3c_2_NA",
                  "C4.5a_2_NA", "C4.5a_8_NA", "C6.10_8_NA", "C6.4a_1_NA", "C6.5_3_1", "C6.5_3_11", "C6.5_3_12",
                  "C6.5_3_4", "C6.5_3_5", "C6.5_3_6", "C6.5_3_7", "C6.5_5_1",	"C6.5_5_10", "C6.5_5_11",	"C6.5_5_12",
                  "C6.5_5_13", "C6.5_5_14", "C6.5_5_15", "C6.5_5_16",	"C6.5_5_17",	"C6.5_5_2", "C6.5_5_3",	"C6.5_5_4",
                  "C6.5_5_5",	"C6.5_5_6",	"C6.5_5_7",	"C6.5_5_8",	"C6.5_5_9")
df_text <- df[text_columns]

# Calculate basic statistics
df_nchar <- data.frame(lapply(df_text, nchar))
sort(colSums(df_nchar, na.rm = T), decreasing = T)
# Remove companies without text responses
df_text <- df_text[rowSums(df_nchar, na.rm = T) > 0,]

# Select only 1 columns
text_column <- data.frame(df_text$C2.4a_16_NA)
text_column <- data.frame(text_column[apply(FUN=nchar, text_column, 1) > 0,])


install.packages("deaR")
library(deaR)

data("Coll_Blasco_2006")
data_example <- read_data(Coll_Blasco_2006,
                          ni=2,
                          no=2)

export(Coll_Blasco_2006, "Coll_Blasco_2006.xlsx")



