#SCRIPT TO CREATE BREEDING SUCCESS AND ENVIRONMENTAL DATA FRAMES FOR EACH TIME SERIES
#Written by Helen Killeen (hkilleen@faralloninstitute.org)

# Fri Sep 13 15:54:35 2024 ------------------------------

#LIBRARIES AND SOURCES
#====
source(file = "scripts/0_Seabird_Helpers.R")#load functions

dat <- readRDS("data/dat.Rda")#load breeding success data

Nfilter <- readxl::read_xlsx("data/metadata/northSeasonalFilter.xlsx")#get seasonal filtering metadata, this table is used to sort out which months of the year are essential for environmental averaging of the pre-breeding and breeding seasons. See Table S2 in the Supplemental Materials. 
#====

#SETUP
#====
sppsite_list <- distinct(dat, sppsite, ECOREGION)#get full list of sppsites
sppsite_list$file <- gsub('/', '', paste(gsub(" ", "_", sppsite_list$ECOREGION), "_MEOW.csv", sep = ""))#add a file ID to link with environmental data

dat_files <- list.files("data/raw/ecoregion_environment/")#get list of GLORYS-12 environmental data files 
#====

#LOOP
#====
#Loop to link colony time series with local ecoregional environmental data (mixed layer temperature, water column stratification [PEA], and chlorophyll-a content [not used in this analysis]) extracted from the GLORYS-12 reanalysis model
for(i in 1:nrow(sppsite_list)){
  #Filter datasets to relevant sppsites and years
  x <- read_csv(paste("data/raw/ecoregion_environment/", sppsite_list$file[i], sep = ""))#read in temperature data
  x$year <- year(x$time)#extract year
  x$month <- month(x$time)#extract month
  
  y <- droplevels(dplyr::select(filter(dat, sppsite == sppsite_list$sppsite[i]), year, spp, site, sppsite, PROVINCE, ECOREGION, bs, pr_failure, nyear))#filter dat to relevant time series and drop irrelevant factor levels
  
  z <- filter(Nfilter, sppsite == sppsite_list$sppsite[i])#get breeding season start and end months for time series
  
  #Add in environmental averages
  if(z$total_length[1]>0){
    y <- left_join(y, summarize(group_by(filter(x, month>=z$pre_breeding_start, month<=z$pre_breeding_end), year), pre_breeding_temp = mean(thetao)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$breeding_start, month<=z$breeding_end), year), breeding_temp = mean(thetao)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$pre_breeding_start, month<=z$pre_breeding_end), year), pre_breeding_chl = mean(chl)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$breeding_start, month<=z$breeding_end), year), breeding_chl = mean(chl)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$pre_breeding_start, month<=z$pre_breeding_end), year), pre_breeding_strat = mean(PEA)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$breeding_start, month<=z$breeding_end), year), breeding_strat = mean(PEA)), by = "year")#get annualized pre-breeding temperature mean
  }else{
    y <- left_join(y, summarize(group_by(filter(x, month>=z$pre_breeding_start, month<=z$pre_breeding_end), year), pre_breeding_temp = mean(thetao)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$pre_breeding_start, month<=z$pre_breeding_end), year), pre_breeding_chl = mean(chl)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$pre_breeding_start, month<=z$pre_breeding_end), year), pre_breeding_strat = mean(PEA)), by = "year")#get annualized pre-breeding temperature mean
    
    for(j in 1:nrow(x)){
      if(x$month[j]>10){
        x$year[j] <- x$year[j]+1
      }
    }
    y <- left_join(y, summarize(group_by(filter(x, month>=z$breeding_start|month<=z$breeding_end), year), breeding_temp = mean(thetao)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$breeding_start|month<=z$breeding_end), year), breeding_chl = mean(chl)), by = "year")#get annualized pre-breeding temperature mean
    y <- left_join(y, summarize(group_by(filter(x, month>=z$breeding_start|month<=z$breeding_end), year), breeding_strat = mean(PEA)), by = "year")#get annualized pre-breeding temperature mean
  }
  
  #Scale/normalize and detrend data
y <- drop_na(y, bs) %>%  # Drop NAs from the response
    mutate(stbs = scale(bs), # Compute scaled breeding success
           stpb_temp = scale(pre_breeding_temp),
           stb_temp = scale(breeding_temp),
           stpb_strat = scale(pre_breeding_strat),
           stb_strat = scale(breeding_strat),
           stpb_chl = scale(pre_breeding_chl),
           stb_chl = scale(breeding_chl)) # Compute number of data points
  
y <- y[complete.cases(y),]
y <- y %>% 
    mutate(d.stbs = detrend(y$stbs, order = 2),
           d.stpb_temp = detrend(stpb_temp, order = 2),
           d.stpb_chl = detrend(stpb_chl, order = 2),
           d.stpb_strat = detrend(stpb_strat, order = 2),
           d.stb_temp = detrend(stb_temp, order = 2),
           d.stb_chl = detrend(stb_chl, order = 2),
           d.stb_strat = detrend(stb_strat, order = 2))
    
#save data frame
  write_csv(as.data.frame(as.matrix(y)), file = paste("data/sppsite_environment/", sppsite_list$sppsite[i], ".csv", sep = ""))
}

#bind all breeding success/environment time series into a single data frame
files  <- list.files("data/sppsite_environment/", full.names = TRUE)
tables <- lapply(files, read_csv)
combined.df <- do.call(rbind , tables)
combined.df <- combined.df %>% mutate_if(is.character, as.factor)#convert new variables to factors

#save dataframe to output for plotting
saveRDS(combined.df, "data/combined.df.rds")
#====

#Go to 3_GLMM_trends