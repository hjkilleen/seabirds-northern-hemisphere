#Frequentist modeling of seabird breeding success and environmental trends by ecosystem
#Helen Killeen, adapted from code written by Dave Shoeman

#Note that while chlorophyll-a content trends are calculated in this script, these trends are not included in the published manuscript
#Supplementary tables for models are printed in HTML format using the kableExtra() package

# Thu Sep 19 13:54:09 2024 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/0_Seabird_Helpers.R")
dat <- readRDS("data/dat.rda")
#=====

#SET UP TO CALCULATE ENVIRONMENTAL TRENDS BY ECOSYSTEM
#====
region_list <- unique(dplyr::select(dat, ECOREGION, PROVINCE))#get full list of ecoregions and provinces
region_list$file <- paste("data/ecoregion_environment/", gsub("/", "", gsub(" ", "_", region_list$ECOREGION)), "_MEOW.csv", sep = "")#add a file ID to link with environmental data

dat_files <- list.files("data/ecoregion_environment/")#get list of GLORYS-12 environmental data files 

out1 <- list()
for(i in 1:length(dat_files)){#loop to create a giant data frame with all environmental measurements for all ecoregions
  #Filter datasets to relevant sppsites and years
  x <- read_csv(paste("data/ecoregion_environment/", dat_files[i], sep = ""))#read in temperature data
  x$year <- year(x$time)#extract year
  x$month <- month(x$time)#extract month
  x$file <- rep(paste("data/ecoregion_environment/", dat_files[i], sep = ""), nrow(x))
  y <- left_join(x, region_list)
  out1[[i]] <- y
}
df1 <- bind_rows(out1)

#Create new dataframe with mean conditions during the PRE-BREEDING season
df.pb <- df1 %>% #months used in filtering are justified on the basis of resident bird species breeding phenology, see table S2 in supplement
  filter(case_when(PROVINCE == "Arctic" ~ month >= 3 & month <= 5,
                   PROVINCE == "Cold Temperate Northeast Pacific" ~ month >= 3 & month <= 5,
                   PROVINCE == "Cold Temperate Northwest Pacific" ~ month >= 3 & month <= 5,
                   PROVINCE == "Cold Temperate Northwest Atlantic" ~ month >= 3 & month <= 5,
                   PROVINCE == "Warm Temperate Northeast Pacific" ~ month >= 1 & month <= 2,
                   PROVINCE == "Hawaii" ~ month >= 8 & month <= 11,
                   PROVINCE == "Northern European Seas" ~ month >= 2 & month <= 5,)) %>% 
  group_by(year, PROVINCE, ECOREGION) %>% 
  summarise(temp = mean(thetao), strat = mean(PEA), chl = mean(chl)) %>% 
  as.data.frame()#compute seasonal mean values per ecoregion
out <- list() 

for(i in levels(df.pb$ECOREGION)) {#compute scales values
  d <- filter(df.pb, ECOREGION == i) %>% # For each time series
    mutate(st.temp = scale(temp), # Compute scaled temperature
           st.strat = scale(strat), # Compute scaled stratification
           st.chl = scale(chl)) # Compute scaled chlorophyll
  out[[i]] <- d
}
df.pb <- bind_rows(out) # Bind the elements of the list back into a data frame
df.pb$yearno <- df.pb$year-min(df.pb$year)#add year index
saveRDS(df.pb, file = "data/df.pb.rds")#save file

#Create new dataframe with mean conditions during the BREEDING season
df.b <- df1 %>% #months used in filtering are justified on the basis of resident bird species breeding phenology, see table S2 in supplement
  filter(case_when(PROVINCE == "Arctic"~ month >= 6 & month < 9,
                   PROVINCE == "Cold Temperate Northeast Pacific" ~ month >= 6 & month <= 9,
                   PROVINCE == "Cold Temperate Northwest Pacific" ~ month >= 6 & month <= 8,
                   PROVINCE == "Cold Temperate Northwest Atlantic" ~ month >= 6 & month <= 8,
                   PROVINCE == "Warm Temperate Northeast Pacific" ~ month >= 3 & month <= 7,
                   PROVINCE == "Hawaii" ~ month %in% c(1:5, 12),
                   PROVINCE == "Northern European Seas" ~ month >= 5 & month <= 8,))

df.b.h <- filter(df.b, PROVINCE == "Hawaii")#group Hawaii December with the following year as birds breed during the boreal winter months
for(j in 1:nrow(df.b.h)){
  if(df.b.h$month[j]>10){
    df.b.h$year[j] <- df.b.h$year[j]+1
  }
}
df.b <- rbind(filter(df.b, PROVINCE != "Hawaii"), df.b.h)#recombine dataframe with Hawaii edit

df.b <- df.b %>% 
  group_by(year, PROVINCE, ECOREGION) %>% 
  summarise(temp = mean(thetao), strat = mean(PEA), chl = mean(chl)) %>% 
  as.data.frame()#compute seasonal mean values per ecoregion
out <- list() 

for(i in levels(df.b$ECOREGION)) {#compute scales values
  d <- filter(df.b, ECOREGION == i) %>% # For each time series
    mutate(st.temp = scale(temp), # Compute scaled temperature
           st.strat = scale(strat), # Compute scaled stratification
           st.chl = scale(chl)) # Compute scaled chlorophyll
  out[[i]] <- d
}
df.b <- bind_rows(out) # Bind the elements of the list back into a data frame
df.b$yearno <- df.b$year-min(df.b$year)#add year index
saveRDS(df.b, file= "data/df.b.rds")#save file
#====

#MODEL ENVIRONMENTAL TRENDS
#====
#Create generalized linear mixed effects (hierarchical) models for long-term trends in mixed layer temperature, water column stratification, and chlorophyll-a content for each northern hemisphere ecosystem during the pre-breeding and breeding seasons. 

#MODEL PRE-BREEDING SEASON ENVIRONMENTAL TRENDS
#Mixed layer temperature
pb.temp.trends <- lme(st.temp ~ yearno + PROVINCE + yearno:PROVINCE,
                random = ~yearno|ECOREGION, # Allow random slopes by time series
                control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                method = "ML",
                data = df.pb)
saveRDS(pb.temp.trends, file = "output/pb.temp.trends.rds")#save model

#Water column stratification
pb.strat.trends <- lme(st.strat ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|ECOREGION, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = df.pb)
saveRDS(pb.strat.trends, file = "output/pb.strat.trends.rds")#save model

#Chlorophyll-a content
pb.chl.trends <- lme(st.chl ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|ECOREGION, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = df.pb)
saveRDS(pb.chl.trends, file = "output/pb.chl.trends.rds")#save model

#Create full model summaries for pre-breeding trends. HTML tables are included as supplementary tables S5-S8
tab_model(pb.temp.trends)#S5
tidy(emtrends(pb.temp.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #S6
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()
tab_model(pb.strat.trends)#S7
tidy(emtrends(pb.strat.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #S8
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()
tab_model(pb.chl.trends)#Not included in published data
tidy(emtrends(pb.chl.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #Not included in published data
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()

#MODEL BREEDING SEASON ENVIRONMENTAL TRENDS
#Mixed layer temperature
b.temp.trends <- lme(st.temp ~ yearno + PROVINCE + yearno:PROVINCE,
                      random = ~yearno|ECOREGION, # Allow random slopes by time series
                      control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                      method = "ML",
                      data = df.b)
saveRDS(b.temp.trends, file = "output/b.temp.trends.rds")#save model

#Water column stratification
b.strat.trends <- lme(st.strat ~ yearno + PROVINCE + yearno:PROVINCE,
                       random = ~yearno|ECOREGION, # Allow random slopes by time series
                       control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                       method = "ML",
                       data = df.b)
saveRDS(b.strat.trends, file = "output/b.strat.trends.rds")#save model

#Chlorophyll-a content
b.chl.trends <- lme(st.chl ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|ECOREGION, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = df.b)
saveRDS(b.chl.trends, file = "output/b.chl.trends.rds")#save model

#Create full model summaries for pre-breeding trends. HTML tables are included as supplementary tables S9-12
tab_model(b.temp.trends)#S9
tidy(emtrends(b.temp.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #S10
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()
tab_model(b.strat.trends)#S11
tidy(emtrends(b.strat.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #S12
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()
tab_model(b.chl.trends)#Not included in published data
tidy(emtrends(b.chl.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #Not included in published data
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()
#====

#Corresponding figure produced in figure_environmental_trends



#MODEL BREEDING SUCCESS TRENDS
#====
#Create generalized linear mixed effects (hierarchical) model for long-term trends in seabird breeding success for each northern hemisphere ecosystem. 

n.trends <- lme(stbs ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|sppsite, # Allow random slopes by time series
                     correlation = corCAR1(form = ~ yearno|sppsite), # Incorporate temporal autocorrelation in discontinuous time series, by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = dat)

saveRDS(n.trends, file = "output/n.trends.rds")#save model

#Create full model tables for seabird breeding success trends. Code generated HTML tables that are included as supplementary Tables S13-14
tab_model(n.trends)#S13
tidy(emtrends(n.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) %>% #S14
  kbl(caption = "Using 95% confidence interval", digits = 3) %>% 
  kable_classic()
#====

#Corresponding figure produced in figure_seabird_trends

#Go to 4_GLMM_Biophysical
