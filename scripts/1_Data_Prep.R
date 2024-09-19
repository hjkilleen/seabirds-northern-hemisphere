# LOAD AND PREPARE BREEDING SUCCESS DATA FOR ANALYSIS

# Written by Helen Killeen (hkilleen@faralloninstitute.org), based on code written by David Schoeman (david.schoeman@gmail.com), Brian Hoover (bhoover@faralloninstitute.org), Bill Sydeman (wsydeman@comcast.net) and Sarah-Ann Thompson (sathompson@faralloninstitute.org)

# Fri Sep 13 15:13:09 2024 ------------------------------


# Source the functions ---------------------------------------------------------
	source(file = "scripts/0_Seabird_Helpers.R")


# Load the data, combine and prep ----------------------------------------------

	dat1 <- read_xlsx("data/data_breedingsuccess_north.xlsx") %>% #load northern hemisphere subset of the Global Breeding Success Database
		rename(year_s = year_s_hemi,# Rename variables so that they're slightly easier to deal with
		       year_of_record = year,
		       year = year_curated,
					 site = site,
					 lat = latitude,
					 lon = longitude ,
					 sppno = speciesnum,
					 spp = species,
					 sppcode = bird_code,
					 bs = breeding_success,
					 sample_size = n,
					 notes = notes,
					 exclude = exclude_data)
	dat1$spp[dat1$spp == 'black-legged kittiwake, harbour colony'] <- 'black-legged kittiwake'#remove cliff BLKI colony from Rost, retain the nearby harbour colony
	dat1$spp[dat1$spp == 'black-legged kittiwake1'] <- 'black-legged kittiwake'#remove BLKI time series 2 from Hornoya, retain the nearby time series 1
	dat1 <- filter(dat1, site != "Ebro Delta")#eclude Ebro Delta site as it is the only site in the Mediterranean ecosystem and can't be adequately compared to other, more site-rich ecosystems using this approach
	dat1$exclude[is.na(dat1$exclude)] <- "FALSE"#exclude entries with strong top-down influence or DDT impact on breeding success
	dat1 <- filter(dat1, !exclude=="TRUE")
	dat1 <- mutate(dat1, sppsite = as.factor(paste(spp, site, sep = "_"))) # Create a new factor variables per time series
	dat1 <- mutate(dat1, Hemisphere = lat)#create label for northern and southern hemisphere sites in the dataset
	for(i in 1:nrow(dat1)){
	  if(dat1$Hemisphere[i]>0){
	  dat1$Hemisphere[i] <- "North"
	  } else{
	  dat1$Hemisphere[i] <- "South"
	  }}
	dat1$yearno <- as.numeric(dat1$year)-min(dat1$year)#add serial variable for year
		sum(duplicated(dat1[, -c(1:2)])) # Check to see whether there are any duplicates: none found
		
	# Merge in the trophic levels
		dat2 <- left_join(dat1, read.csv("data/Lookup_table.csv", stringsAsFactors = TRUE),
											by = c("spp" = "Species")) %>%  # Merge Biome and TL into dat
			rename(Depth = Foraging.depth, spp.name = Scientific.name, TL = Trophic.level)
		  dat2$Hemisphere <- as.factor(dat2$Hemisphere)
			dat2$TL <- reFactor(dat2$TL, c(3, 1, 2)) # Rearrange levels to more coherent tropic distribution
			dat2$Depth <- reFactor(dat2$Depth, c(2, 1)) # Rearrange Depth levels
				levels(dat2$Depth) <- c("Surface", "Deep") # Capitalize the factor levels
				levels(dat2$Hemisphere) <- c("North", "South")
			dat2 <- filter(dat2, !is.na(TL))
	
	# Merge in ecoregional and ecosystem (province) classifications
			#Get Marine Ecoregions of the World shapefiles
			# Load and rasterise shape file
			meow <- st_read("data/MEOW/meow_ecos.shp") # Read in the shape file (polygons) downloaded from https://www.worldwildlife.org/publications/marine-ecoregions-of-the-world-a-bioregionalization-of-coastal-and-shelf-areas
			head(meow) # What's in there? A spatial data frame with some descriptors and geometries
			meow %>% group_by(ECOREGION) %>% summarise(no = n_distinct(ECO_CODE_X)) %>% as.data.frame() # Each LME (by name) has a unique LME_NUMBER - good
			r <- raster(res = 0.25) # An empty 0.25° raster...you can make it finer, if you like
			r.meow <- rasterize(meow, r) # Identify which LME is in each raster cell - will take a while
			plot(r.meow) # Plot the MEOWs (1-232)
			
			# Add MEOW ID to dat
			meow.lkup <- dat2 %>% dplyr::select("site", "lon", "lat") %>% unique() #%>% # A lookup table by site and coordinates
			pos <- as.data.frame(meow.lkup[,2:3])#just the positions
			
			new<-meow.lkup %>% mutate(no = extractnearestcoast(r.meow, pos), # What row number in the sf does this correspond with? Note that some coordinates are "on land" so use custom function
			                          ECOREGION = meow$ECOREGION[no], # Extract the name by row number
			                          no = NULL, # Kill the row number
			                          lon = NULL, # Kill lon
			                          lat = NULL) # Kill lat
			new2<-droplevels(new) #added this because otherwise non-present factor levels were screwing everything up.
			dat2 <- left_join(dat2, new2) # Merge back into breeding success dataset
			dat2 <- left_join(dat2, dplyr::select(as.data.frame(meow), ECOREGION, PROVINCE, REALM)) #include ecoregion, province (a.k.a. ecosystem in manuscript), and ocean realm
			dat2$ECOREGION <- as.factor(dat2$ECOREGION)#make all labels factors
			dat2$REALM <- as.factor(dat2$REALM)
			dat2$PROVINCE <- as.factor(dat2$PROVINCE)

# Create breeding success data required for modelling -------------------------------------------
				
		out <- list() 
		for(i in levels(dat2$sppsite)) {
			d <- filter(dat2, sppsite == i) %>% # For each time series
				drop_na(bs) %>%  # Drop NAs from the response
				mutate(pr_failure = ifelse(bs > mean(bs)*.1, 0, 1), # Code breeding failure
							 stbs = scale(bs), # Compute scaled breeding success
							 nyear = n_distinct(year)) # Compute number of data points
			out[[i]] <- d
			}
		dat <- bind_rows(out) # Bind the elements of the list back into a data frame

# Check against selection criteria and select just the series we need ----------

	# Which series are shorter than the 10-year cut-off?
		dat %>% 
			group_by(sppsite) %>% 
			summarise(minYr = min(year), maxYr = max(year)) %>% 
			mutate(lengthYr = maxYr - minYr + 1) %>% 
			data.frame() %>% 
			arrange(lengthYr)#none, we drop no time series on the basis of length
	
	# Save the data	
		saveRDS(dat, file = "data/dat.Rda")
		
# Extract the summary values to be included in the main text of the manuscript
		nrow(dat)#number of unique observations n=4,023
		length(unique(dat$sppsite))#number of unique time series analyzed n=138
		length(unique(dat$spp))#number of unique species analyzed n=39
		length(unique(dat$site))#number of unique colony sites analyzed n=48

# Make lookup table of sppsite and coordinates ---------------------------------

		xy <- dat %>%
			group_by(sppsite, Hemisphere, TL) %>% # For each time series
			dplyr::summarise(spp = spp[1], # Get the species
								site = site[1], # Get the site
								syr = min(year), # Get the start year
								eyr = max(year), # Get the end year
								x = mean(lon), # Get the longitude
								y = mean(lat), # Get the laitude
								depth = Depth[1], # Get the depth of foraging for that species
								nyear = mean(nyear)) %>% # Get the number of data points
			as.data.frame() # Make it a data frame
		saveRDS(xy, file = "data/xy.Rda")
#=========
		
#Produce study map in figure_map

# Go to 2_Data_Merge.R

		
			
			