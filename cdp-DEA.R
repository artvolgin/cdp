
#################################### Data Envelopment Analysis #############################
library(deaR)
library(ggpubr)

##### 2020

# Read data
cities20_dear <- read_data(datadea = cities20_KPIs,
                           dmus = 2,
                           inputs = c('gdp_per_capita', 'population', 'mean_temp'),
                           outputs = c("Risk_Assessment", "Adaptation_Plan",        
                                       "Adaptation_Actions", "Mitigation_Actions",     
                                       "Mitigation_Plan", "Opportunities",          
                                       "Renewable_Energy_Target", "Additional_Measures",    
                                       "Water", "Social_Equity"),
                           nc_inputs = 1:3)

# Run a basic model accounting for uncontrollable inputs
deaR_model_20 <- model_basic(cities20_dear, orientation = 'oo', rts = 'vrs')
#deaR_boot <- bootstrap_basic(cities20_dear, orientation = 'oo', rts = 'vrs')

##### Graphing

# Barplot of efficient vs. non-efficient
eff_20 <- data.frame(org = deaR_model_20$data$dmunames, eff_20 = 1/efficiencies(deaR_model_20))
eff_20 %>% mutate(iseff = ifelse(eff_20 == 1, 1 , 0)) -> eff_20
eff_20 <- eff_20 %>% left_join(cities20_KPIs[, c('org', 'gdp_per_capita')], by = 'org')
eff_20$gdp_group <- ifelse(eff_20$gdp_per_capita >= mean(eff_20$gdp_per_capita), 1, 0)

p1 <- eff_20  %>% 
  ggplot(aes(x = factor(iseff))) + 
  geom_bar(stat="count", width=0.7) +
  ylab("Count") + xlab("") + 
  scale_x_discrete(labels = c("Non-efficient", "Efficient")) +
  ggtitle("Efficient / Non-Efficient DMUs") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

p2 <- eff_20 %>% filter(iseff == 0) %>%
  ggplot(aes(x = eff_20)) + geom_histogram(bins = 5) +
  ggtitle("Distribution of Non-Efficient DMUs") +
  ylab("Count") + xlab('Efficiency Score for Non-Efficient') 

 ggarrange(p1, p2, ncol = 2, nrow = 1)

# Slacks: heatmap by gdp
# Preparing data
slacks_20 <- as.data.frame(slacks(deaR_model_20))
slacks_20$org <- rownames(slacks_20)
slacks_eff_20 <- eff_20 %>% 
  left_join(slacks_20 %>% select(4:ncol(slacks_20)), by = 'org')
colnames(slacks_eff_20) <- c('org', 'eff_20', 'iseff', 'gdp_per_capita', 'gdp_group',
                          "Risk_Assessment", "Adaptation_Plan",        
                          "Adaptation_Actions", "Mitigation_Actions",     
                          "Mitigation_Plan", "Opportunities",          
                          "Renewable_Energy_Target", "Additional_Measures",    
                          "Water", "Social_Equity")
rownames(slacks_eff_20) <- slacks_eff_20$org

# High GDP
slacks_eff_high_gdp_20 <- slacks_eff_20 %>% filter(iseff == 0 & gdp_group == 1) %>%
  arrange(eff_20) %>% slice(1:30) %>%
  select(-c(org, eff_20, iseff, gdp_per_capita, gdp_group))
slacks_eff_high_gdp_20 <- as.matrix(slacks_eff_high_gdp_20)

# Low GDP
slacks_eff_low_gdp_20 <- slacks_eff_20 %>% filter(iseff == 0 & gdp_group == 0) %>%
  arrange(eff_20) %>% slice(1:30) %>%
  select(-c(org, eff_20, iseff, gdp_per_capita, gdp_group))
slacks_eff_low_gdp_20 <- as.matrix(slacks_eff_low_gdp_20)

# Plotting
heatmap(slacks_eff_high_gdp)
heatmap(slacks_eff_low_gdp)

##### 2019

# Read data
cities19_dear <- read_data(datadea = cities19_KPIs,
                           dmus = 2,
                           inputs = c('gdp_per_capita', 'population', 'mean_temp'),
                           outputs = c("Risk_Assessment", "Adaptation_Plan",        
                                       "Adaptation_Actions", "Mitigation_Actions",     
                                       "Mitigation_Plan", "Opportunities",          
                                       "Renewable_Energy_Target", "Additional_Measures",    
                                       "Water", "Social_Equity"),
                           nc_inputs = 1:3)

# Run a basic model accounting for uncontrollable inputs
deaR_model_19 <- model_basic(cities19_dear, orientation = 'oo', rts = 'vrs')

##### Malmquist index

# Specifying read_data for the index
data_malmquist <- read_malmquist(datadea = cities19_20_KPIs_year,
                               percol = 'year',
                               arrangement = "vertical",
                               dmus = 3,
                               inputs = c('gdp_per_capita', 'population', 'mean_temp'),
                               outputs = c("Risk_Assessment", "Adaptation_Plan",        
                                           "Adaptation_Actions", "Mitigation_Actions",     
                                           "Mitigation_Plan", "Opportunities",          
                                           "Renewable_Energy_Target", "Additional_Measures",    
                                           "Water", "Social_Equity"),
                               nc_inputs = 1:3)

malmquist <- malmquist_index(data_malmquist, orientation = "oo", rts = 'vrs')
mi <- data.frame(t(malmquist$mi))
colnames(mi) <- 'MI'
tc <- as.data.frame(t(malmquist$tc))
colnames(tc) <- 'TC'
pech <- as.data.frame(t(malmquist$pech))
colnames(pech) <- 'PECH'

# Malmquist output 

# Extracting efficiencies for 2019
eff_19 <- as.data.frame(100*1/efficiencies(deaR_model_19))
eff_19$org <- rownames(eff_19)
rownames(eff_19) <- NULL
colnames(eff_19) <- c('eff_19', 'org')

# Multiplying efficiencies for 2020 by 100
eff_20$eff_20 <- 100*eff_20$eff_20

# Combining all pieces of the output
Malmquist_output <- cbind.data.frame(mi, tc, pech) 
Malmquist_output$org <-  rownames(Malmquist_output)
Malmquist_output <- Malmquist_output[, c(4,1:3)]
rownames(Malmquist_output) <- NULL
Malmquist_output <- Malmquist_output %>%
  left_join(eff_20, by = 'org') %>%
  left_join(eff_19, by = 'org') %>% arrange(desc(gdp_per_capita, gdp_group)) %>%
  na.omit(Malmquist_output)

# Selecting relevant columns
Malmquist_output <- Malmquist_output[,c('org', 'MI', 'TC',
                                        'PECH', 'eff_19',
                                        'eff_20', 'gdp_group')]
colnames(Malmquist_output) <- c('org', 'MI', 'TC', 'PECH',
                                'Efficiency 2019', 'Efficiency 2020', 'gdp_group')

# Top 20 rich countries
Malmquist_output_high_gdp <- Malmquist_output %>% filter(gdp_group == 1) %>%
  slice(1:20) %>% select(-gdp_group) %>% arrange(desc(`Efficiency 2019`))
Malmquist_output_high_gdp[,2:6] <- round(Malmquist_output_high_gdp[,2:6], 2)

# Bottom 20 developing countries
Malmquist_output_low_gdp <- Malmquist_output %>% filter(gdp_group == 0) %>%
  slice(1:20) %>% select(-gdp_group) %>% arrange(desc(`Efficiency 2019`))
Malmquist_output_low_gdp[,2:6] <- round(Malmquist_output_low_gdp[,2:6], 2)

# References
references_20 <- references(deaR_model_20)
references_19 <- references(deaR_model_19)








