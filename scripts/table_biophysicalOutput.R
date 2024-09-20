#Summary table displaying output from ecosystem and seasonally specific models of seabird breeding success, temperature, and stratification. Table prepared for manuscript main text. 
#Written by Helen Killeen

# Thu Sep 19 17:37:45 2024 ------------------------------

#SOURCES
#====
#load libraries
source("scripts/0_Seabird_Helpers.R")
#====

#SET UP
#====
ep <- read_xlsx("output/biophysical_models/biophysical_glmm_summary.xlsx")#load table
ep$Ecosystem <- as.factor(ep$Ecosystem)
levels(ep$Ecosystem) <- c("Arctic", "Northern European Seas", "Cold Temperate Northwest Atlantic", "Cold Temperate Northeast Pacific", "Warm Temperate Northwest Pacific", "Cold Temperate Northwest Pacific", "Hawaii")
#====

#TABLE
#====
ep[,-1] %>% #knit table
  kbl(digits = 2) %>% 
  kable_classic() %>% 
  pack_rows(index = table(ep$Ecosystem))
#====
