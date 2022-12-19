#Read libraries

library(tidyverse)
library(skimr)
library(psych)
library(lavaan)
library(semPlot)

#Read the data

data <- readRDS(file = "data.rds")

total <- data %>% 
  rowwise() %>% 
  mutate(total =  sum(c_across(q01:q12)), .keep = c("unused")) %>% 
  ungroup()

data %>% 
  summarise(across(q01:q12, mean)) %>%
  round(digits = 3)

data %>% 
  summarise(across(q01:q12, median)) %>%
  round(digits = 3)

data %>% 
  summarise(across(q01:q12, sd)) %>%
  round(digits = 3)

data %>% 
  summarise(across(q01:q12, min)) %>%
  round(digits = 3)

data %>% 
  summarise(across(q01:q12, max)) %>%
  round(digits = 3)

#Descriptive
total %>% 
  summarise(min(total), max(total), median(total), mean(total),
            sd(total))

#PBC
pbcor <- function(x){
  total.c = total - x
  round(cor(x, total.c),3)} 

rpb <- apply(data, 2, pbcor) %>% 
  as_tibble() %>% 
  rename(pbcor = value)

items <- cbind(items,rpb)

#Correlation

data %>% 
  select(q01 , q04 , q07 , q12, 
         q06 , q10, 
         q02 , q08, 
         q03 , q05,
         q09 , q11) %>% 
  cor() %>% 
  round(digits = 3)




#Skim all the variables

skim(data)

#Reliability

alpha(data) #, check.keys = TRUE)

#PCA
pca_m <- prcomp(data_boost, center = T, scale. = T)
summary(pca_m)
plot(pca_m)

#EFA

efa_m <- fa(data_boost,nfactors = 4)
summary(efa_m)

data_boost <- sample_n(data, size = 200, replace = T)

fa.none <- fa(r=data_boost, 
              nfactors = 4, 
              # covar = FALSE, SMC = TRUE,
              fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax") # none rotation
print(fa.none)

#Parallel
x11()
parallel <- fa.parallel(data_boost)

x11()
parallel <- fa.parallel(data)

KMO(data_boost)
#CFA
#Define the latent constructs

model <- '
    P1 =~ q01 + q04 + q07 + q12
    P2 =~ q06 + q10
    P4 =~ q02 + q08 
    P6 =~ q03 + q05 
    P8 =~ q09 + q11'

m_1 <- cfa(model, data = data)

summary(m_1, standardized=TRUE)

fitmeasures(m_1,fit.measures = c("rmsea","cfi","tli","srmr"))

semPaths(m_1,"std",curvePivot = TRUE,layout = "tree2",
         edge.color="black",edge.label.cex = 1.2)

         