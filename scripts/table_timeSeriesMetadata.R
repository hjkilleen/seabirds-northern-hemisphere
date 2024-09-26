#Tables S1, S3, and S4 Time Series & Prey Metadata Tables for Supplement
#Tables are produced in HTML format and copied into supplement
#Written by Helen Killeen

# Fri Sep 20 15:25:25 2024 ------------------------------

#SOURCE & DATA
#====
source("scripts/0_Seabird_Helpers.R")

t1 <- read_xlsx("data/metadata/time_series_duration_contributors.xlsx")[,-c(5, 7:10)]#load data contributor site metadata table
t2 <- read_xlsx("data/metadata/northSeasonalFilter.xlsx")#load breeing timing
t2.cites <- read_xlsx("data/metadata/northSeasonalFilter.xlsx", sheet = 3)#load phenology citations
t3 <- read_xlsx("data/prey_records.xlsx")#load prey records table
t4 <- read_csv("data/metadata/prey_groupings.csv")#load prey groupings 

dat <- readRDS("data/dat.Rda")#load bs data for filtering time series
#====

#SET UP TABLE S1 - DATA CONTRIBUTORS, TIME SERIES METADATA
#====
#rename columns for easier reading
names(t1) <- c("Site", "Common name", "Time Series Duration", "Number of years", "Data Contributor")

names(t3) <- c("Site", "Latitude", "Longitude", "Location", "Ecoregion", "Ecosystem", "Common name", "Scientific name", "Prey 1", "Prey 2", "Prey 3", "Citation", "Citation_long", "Co-author pers. comm.")

#filter out un-analyzed time series that were dropped in 1_Data_Prep
ts.an <- unique(dat$sppsite)#create vectors for filtering
t1$sppsite <- paste(t1$`Common name`, t1$Site, sep = "_")
t3$sppsite <- paste(t3$`Common name`, t3$Site, sep = "_")
t1 <- filter(t1, sppsite %in% ts.an)#filter
t3 <- filter(t3, sppsite %in% ts.an)
t1 <- t1[,-ncol(t1)]#drop sppsite
t3 <- t3[,-ncol(t3)]

md <- left_join(dplyr::select(t3, Site, 'Common name', 'Scientific name', Latitude, Longitude, Location, Ecoregion, Ecosystem), t1)

md$Ecosystem <- as.factor(md$Ecosystem)
md <- arrange(md, Ecosystem, Ecoregion, Site, `Common name`)#organize columns for presentation
md <- droplevels(md)
md$Latitude <- round(md$Latitude, 1)#shorten location data
md$Longitude <- round(md$Longitude, 1)
#====

#TABLE S1 - TIME SERIES METADATA
#====
md[,-8] %>% #knit table
  kbl() %>% 
  column_spec(3, italic = T) %>% 
  kable_classic(lightable_options = "striped") %>% 
  pack_rows(index = table(md$Ecosystem))
#====

#SET UP TABLE S2 - BREEDING PHENOLOGY
#====
t2.cites$sppsite <- paste(t2.cites$Species, t2.cites$Sites, sep = "_")#create sppsite for joining
t2 <- left_join(t2, t2.cites)#join tables
t2 <- dplyr::select(t2, Species, Sites, pre_breeding_start, pre_breeding_end, breeding_start, breeding_end, Citation)#select only most pertinent columns for supplement
#====

#TABLE S2 - BREEDING PHENOLOGY
#====
t2 %>% 
  kbl() %>% 
  kable_classic(lightable_options = "striped")
#====

#SET UP TABLE S3 - SEABIRD PREY PORTFOLIOS
#====
ep <- dplyr::select(t3, Ecosystem, Site, 'Common name', 'Scientific name', 'Prey 1', 'Prey 2', 'Prey 3', Citation, 'Co-author pers. comm.')
ep$Ecosystem <- as.factor(ep$Ecosystem)
ep <- droplevels(ep)
ep <- arrange(ep, Ecosystem, Site, `Common name`)
#====

#TABLE S3
#====
ep[,-c(1)] %>% #knit table
  kbl() %>% 
  column_spec(3, italic = T) %>% 
  kable_classic(lightable_options = "striped") %>% 
  pack_rows(index = table(ep$Ecosystem))
#====

#SET UP TABLE S4 - PREY CATEGORIZATION AND GROUPING
#====
names(t4) <- c("Reported Prey ID", "Standardized Prey ID", "Prey Group")
t4 <- t4[order(t4$'Reported Prey ID'),]
#====

#TABLE S4
#====
t4 %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped")
#====

#CIATIONS LIST
#====
cit <- t3$Citation_long
cit <- sort(cit)
cit <- unique(cit)
cit3 <- data.frame(index = 1:length(cit),
                   cit = cit)
cit3[,-1] %>% #knit table
  kbl() %>% 
  kable_classic(lightable_options = "striped")
#====