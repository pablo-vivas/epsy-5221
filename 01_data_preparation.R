#Read libraries

library(tidyverse)
library(readr)
<<<<<<< HEAD
library(stringr)
library(openxlsx)
=======
library(corrplot)
library(skimr)
library(psych)
library(lavaan)
library(semPlot)
>>>>>>> 3446e4c600bf2f13b6434722dea893456b33b389

#Read the data

data <- read_csv("data_qualtrics.csv")

time <- as.numeric(data$`Duration (in seconds)`[-c(1,2,3,14)])

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

colnames(data) <- c(str_c("q0", c(1:9)),str_c("q", c(10:12))) 
saveRDS(data, file = "data.rds")

write.xlsx(data, "data.xlsx", col.names = TRUE, 
           row.names = FALSE, append = FALSE, keepNA = TRUE)

<<<<<<< HEAD

table(data$q01)

x <- apply(data, 2, sd)
sort(x)
=======
semPaths(m_1, 
         "std", 
         curvePivot = TRUE,
         layout = "tree2")
>>>>>>> 3446e4c600bf2f13b6434722dea893456b33b389
