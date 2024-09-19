#Tables S1 and S2
#Time Series & Prey Metadata Tables for Supplement
# Tue Apr  2 15:43:49 2024 ------------------------------

#LIBRARIES
#====
library(readxl)
library(knitr)
library(kableExtra)
#====

#LOAD DATA
#====
t1 <- read_xlsx("data/Tables_SOM.xlsx")[,-c(5, 7:10)]#load site metadata table
t2 <- read_xlsx("data/Table_SM1_sites_birds.xlsx")[,-c(13, 14)]#load prey metadata table
t3 <- read_csv("data/prey/prey_groupings.csv")
#====

#SET UP TABLE S1
#====
names(t2) <- c("Site", "Latitude", "Longitude", "Location", "Ecoregion", "Ecosystem", "Common name", "Scientific name", "Prey 1", "Prey 2", "Prey 3", "Citation", "Citation_long", "Hoover pers. comm.")
names(t1) <- c("Site", "Common name", "Time Series Duration", "Number of years", "Data Contributor")

md <- left_join(dplyr::select(t2, Site, 'Common name', 'Scientific name', Latitude, Longitude, Location, Ecoregion, Ecosystem), t1)

md$Ecosystem <- as.factor(md$Ecosystem)
md <- arrange(md, Ecosystem, Ecoregion, Site, `Common name`)#organize columns for presentation
md <- filter(md, Ecosystem != "Mediterranean Sea")#exclude Mediterranean data since it was not analyzed in this study
md <- droplevels(md)
md$Latitude <- round(md$Latitude, 1)#shorten location data
md$Longitude <- round(md$Longitude, 1)
#====

#TABLE S1
#====
md[,-8] %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped") %>% 
  pack_rows(index = table(md$Ecosystem))
#====

#SET UP TABLE S2
#====
ep <- dplyr::select(t2, Ecosystem, Site, 'Common name', 'Scientific name', 'Prey 1', 'Prey 2', 'Prey 3', Citation, 'Hoover pers. comm.')
ep$Ecosystem <- as.factor(ep$Ecosystem)
ep <- filter(ep, Ecosystem != "Mediterranean Sea")#exclude Mediterranean data since it was not analyzed in this study
ep <- droplevels(ep)
ep <- arrange(ep, Ecosystem, Site, `Common name`)
#====

#TABLE S2
#====
ep[,-c(1)] %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped") %>% 
  pack_rows(index = table(ep$Ecosystem))
#====

#SET UP TABLE S3
#====
names(t3) <- c("Reported Prey ID", "Standardized Prey ID", "Prey Group")
t3 <- t3[order(t3$'Reported Prey ID'),]
#====

#TABLE S3
#====
t3 %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped")
#====

#CIATIONS LIST
#====
cit <- t2$Citation_long
cit <- sort(cit)
cit <- unique(cit)
cit2 <- data.frame(index = 1:length(cit),
                   cit = cit)
cit2[,-1] %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped")
#====