#Table 1
#Environmental Predictors Table
# Wed Nov 29 11:23:49 2023 ------------------------------

#LIBRARIES
#====
library(readxl)
library(knitr)
library(kableExtra)
#====

#SET UP
#====
ep <- read_xlsx("output/env.glmm.xlsx")#load table
#====

#TABLE
#====
ep[,-1] %>% #knit table
  kbl() %>% 
  kable_classic() %>% 
  pack_rows(index = table(ep$Ecosystem))
#====