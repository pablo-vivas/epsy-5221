#Read libraries

library(tidyverse)
library(readr)
library(corrplot)
library(skimr)
library(psych)
library(lavaan)
library(semPlot)

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
corrplot(cor(data), method = 'number', )

#Skim all the variables

skim(data)

#Reliability

alpha(data)

#PCA
pca_m <- prcomp(data, center = T, scale. = T)
summary(pca_m)
plot(pca_m)

#EFA

efa_m <- fa(data,nfactors = 3, rotate = "varimax")
summary(efa_m)

#Parallel
parallel <- fa.parallel(data)

#CFA
#Define the latent constructs

model <- '
    P1 =~ Q6_1 + Q6_4 + Q6_7 + Q6_12
    P2 =~ Q6_6 + Q6_10
    P4 =~ Q6_2 + Q6_8 
    P6 =~ Q6_3 + Q6_5 
    P8 =~ Q6_9 + Q6_11'

m_1 <- cfa(model, data = data)

summary(m_1, standardized=TRUE)
fitmeasures(m_1,fit.measures = c("rmsea","cfi","tli","srmr"))

semPaths(m_1, 
         "std", 
         curvePivot = TRUE,
         layout = "tree2")
