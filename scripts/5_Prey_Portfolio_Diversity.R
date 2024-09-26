#Script to calculate Shannon-Wiener diversity indices for each ecosystem based on literature-derived predator-prey pairings
#Written by Helen Killeen & Brian Hoover

# Thu Sep 26 12:41:44 2024 ------------------------------

#SOURCES & DATA
#====
source("scripts/0_Seabird_Helpers.R")

prey.df <- readRDS("data/prey.df.rds")#load cleaned prey dataframe
prey.df2 <- readRDS("data/prey.df.proportional.rds")#load proportional dataframe
#====

#CALCULATE SHANNON-WIENER INDICES PER ECOSYSTEM
#====
species_count <- prey.df %>%#count the number of seabird colonies (sppsites) that use each prey category per ecosystem
  group_by(province, category) %>%
  summarise(count = n()) %>%
  ungroup()

#convert to a community matrix with ecosystems as rows
species_matrix <- with(species_count, tapply(count, list(province, category), sum, default = 0))

# Calculate the Shannon-Weaver Diversity Index
diversity_indices <- apply(species_matrix, 1, function(x) diversity(x))#calculate index
diversity_df <- data.frame(province = names(diversity_indices), Shannon_Index = diversity_indices)

saveRDS(diversity_df, "output/diversity_df.rds")#save as output for plotting
#====

#CALCULATE PROPORTIONAL PREY-USE PER ECOSYSTEM
#====
prey.df2 <- prey.df2[order(prey.df2[,1], -prey.df2[,3]),]#sort proportional prey by ecosystem and proportion
prey.df2 <- filter(prey.df2, value != 0)#eliminate 0s, this gives us a list of the prey items on which seabirds rely in each ecosystem sorted by importance. Proportions indicate the proportion of seabird colonies that exploit each prey resource as one of its top three resources. 
kbl(prey.df2) %>% #print HTML table to populate values shown in Figure 6 of main text
  kable_classic()
#====

#Plot prey portfolios using figure_prey_portfolios