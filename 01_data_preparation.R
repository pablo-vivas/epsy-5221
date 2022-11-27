#Read libraries

library(tidyverse)
library(readr)
library(corrplot)

#Read the data

data <- read_csv("data_qualtrics.csv")

#Select the rows and columns needed
data <- data %>% 
  filter(!row_number() %in% c(1,2)) %>% 
  select(Q6_1:Q6_12) %>% 
  drop_na() %>% 
  mutate_if(is.character, as.factor)

data %>% summary()

#Recode to numbers
data <- data %>% 
  mutate_at(vars(Q6_1:Q6_12), list(~recode(.,'Strongly disagree' = 1, 
                                           'Disagree' = 2, 
                                           'Neutral' = 3,
                                           'Agree' = 4,
                                           'Strongly Agree' = 5)))

#Reverse order of item 8
data$Q6_8 <- 6 - data$Q6_8

#Correlation
corrplot(cor(data), method = 'number')
