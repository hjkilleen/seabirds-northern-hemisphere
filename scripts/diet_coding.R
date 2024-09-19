#Script to plot prey diversity index figure for manuscript
#Helen Killeen & Brian Hoover

# Fri Nov 17 13:03:17 2023 ------------------------------

#LIBRARIES AND SOURCES
#====
library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(cowplot)
library(readxl)
#library(officer)
#library(flextable)
library(vegan)
library(gridExtra)

prey.df <- read_xlsx("data/Table_SM1_sites_birds.xlsx")#load data
prey.grp <- read_csv("data/prey/prey_groupings.csv")
#====

#SET UP
#====
prey.df <- prey.df[,c(1,5,6,8:11)]
names(prey.df) <- c("site", "ecoregion", "province", "spp", "prey.1", "prey.2", "prey.3")
prey.df$sppsite <- paste(prey.df$spp, prey.df$site, sep = "_")
prey.df <- melt(prey.df, id = c("sppsite", "site", "ecoregion", "province", "spp"))
prey.df <- filter(prey.df, !is.na(value), value != "NA", !province %in% c("Mediterranean Sea", "Hawaii", "Warm Temperate Northeast Pacific"))#filter to only the provinces where we have the most bird time series and exclude NAs

# prey.df <- filter(prey.df, ecoregion %in% c("Northern Norway and Finmark", "Northern California", "North Sea", "Gulf of Maine/Bay of Fundy", "Gulf of Alaska", "Eastern Bering Sea", "Aleutian Islands", "Sea of Okhotsk", "Hawaii"))#filter to only the ecoregions where we have the most bird time series (n>4)

prey.df <- left_join(prey.df, prey.grp)#join in prey groupings

#set up data for proportional predator-prey pairings
prey.df2 <-dcast(prey.df, sppsite + province~category, length) %>% 
  group_by(province) %>% 
  summarize_if(is.numeric, sum)#number of birds that prey on each group per province

# #set up data for proportional predator-prey pairings
# prey.df2 <-dcast(prey.df, sppsite + ecoregion~category, length) %>% 
#   group_by(ecoregion) %>% 
#   summarize_if(is.numeric, sum)#number of birds that prey on each group per province

prey.df2$total <- rowSums(prey.df2[,-1])#total number of time series per province
prey.df2 <- prey.df2 %>% mutate(across("Ammodytidae":"Zooplankton", ~ .x/total))#get proportion of birds relying on each prey item by province (i.e., prey use index)
prey.df2 <- melt(prey.df2[,-33], id = "province")#reshape to tall format

# prey.df2 <- melt(prey.df2[,-26], id = "ecoregion")#reshape to tall format


##############################################################
### PART 1 plot the prey diversity per PROVINCE
##############################################################


# Define species groups for plotting
green_species <- c("Clupeidae",  "Sardinops", "Scomberidae", "Engraulidae", "Osmeridae", "Scomberesocidae", "Mallotus", "Stromateidae", "Ammodytidae")#Pelagic schooling fish
#light_blue_species <- c("Scorpaenidae")
blue_species <- c("Cottidae", "Hexagrammidae", "Pholidae", "Pleuronectiformes","Gadidae", "Paralichthyidae", "Scorpaenidae")#Nearshore demersal fish
purple_species <- c("Myctophidae")#Myctophids
#light_purple_species <- c("Ammodytidae")
red_species <- c("Copepoda", "Amphipoda", "Euphausiidae")#Macrozooplankton
yellow_species <- c("Cephalopoda")#Squid
gray_species <- c("Aves", "Variable", "Gelatinous", "Kleptoparasitism", "Polychaeta", "Unid Fish", "Unid Invertebrate", "Zooplankton", "Polyplacophora", "Crustacea")#Other

# Create color vectors for each group of species
# Adjust these colors to preference
green_colors <- colorRampPalette(c("#edf8e9ff","#41ab5dff"))(length(green_species))
blue_colors <- colorRampPalette(c("#eff3ffff","#08519cff"))(length(blue_species))   
#light_blue_colors <- colorRampPalette(c("#0051f2ff","#0051f2ff"))(length(light_blue_species)) # Single color for light blue
purple_colors <- colorRampPalette(c("#8573cbff","#8573cbff"))(length(purple_species))  # Single color for purple
#light_purple_colors <- colorRampPalette(c("#bcbddcff","#bcbddcff"))(length(light_purple_species))  # Single color for light purple
red_colors <- colorRampPalette(c( "#fee0d2ff","#de2d26ff"))(length(red_species))
yellow_colors <- colorRampPalette(c("#fec44f","#fec44f"))(length(yellow_species))  # Single color for yellow
gray_colors <- colorRampPalette(c( "#f7f7f7ff","#252525ff"))(length(gray_species))

# Combine into a single named vector
col_vector <- setNames(c(blue_colors, green_colors, purple_colors, red_colors, yellow_colors, gray_colors), c(blue_species, green_species, purple_species, red_species, yellow_species,gray_species))

#order the vector
all_species <- rev(c(blue_species, green_species, purple_species,red_species, yellow_species, gray_species))
prey.df$category <- factor(prey.df$category, levels = all_species)

#ggplot code for counts of predator-prey pairings per province
#create x axis labels
new_labels <- c("Cold Temperate Northeast Pacific" = "Cold Temperate\nNortheast Pacific",
                "Cold Temperate Northwest Atlantic" = "Cold Temperate\nNorthwest Atlantic",
                "Cold Temperate Northwest Pacific" = "Cold Temperate\nNorthwest Pacific",
                "Northern European Seas" = "Northern European\nSeas",
                "Warm Temperate Northeast Pacific" = "Warm Temperate\nNortheast Pacific")

# Raw prey use barplot for supplement
original_plot <- ggplot(prey.df, aes(x = province, fill = category)) +
  geom_bar(color = "white") +  # Add white lines between stacks
  scale_fill_manual(values = col_vector) +
  scale_x_discrete(labels = new_labels) +
  theme_classic(base_size = 15) + xlab("Ecosystem") + ylab("# of Prey Types") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")  # Hide the original legend
original_plot

#legend stuff
legend_categories <- c("Other", "Squid", "Crustacean zooplankton","Mesopelagic fish","Pelagic schooling fish","Nearshore and demersal fish")
legend_colors <- c("#bdbdbd","#fec44f","#de2d26ff","#8573cbff","#41ab5dff","#08519cff")

#prepare an annotated plot
annotated_plot <- original_plot

#set adjustments
x_start <- 2.65  # Adjust as needed
y_start <- 75   # Starting point on the y-axis
y_step <- 4     # Space between legend items
y_thick <- 2    # Thickness of legend color band

# Add colored boxes and text for each category
for(i in 1:length(legend_categories)) {
  y_position <- y_start - (i * y_step)
  annotated_plot <- annotated_plot +
    annotate("rect", xmin = x_start, xmax = x_start + 0.5, ymin = y_position, ymax = y_position + y_thick, 
             fill = legend_colors[i]) +
    annotate("text", x = x_start + 0.6, y = y_position + 0.5, label = legend_categories[i],
             hjust = 0, size = 5)  #
}
annotated_plot
# Adjust the plot margins if needed

#save
ggsave("figures/raw_prey_counts.jpg", plot = annotated_plot, dpi = 300, width = 10, height = 8, units = "in")



##############################################################
###PART 2 calculate the diversity indices.  Calculating SW index first, then Margalef. Using Margalef
##############################################################

#set up data to calculate indices
species_count <- prey.df %>%
  group_by(province, category) %>%
  summarise(count = n()) %>%
  ungroup()

#use matrix
species_matrix <- with(species_count, tapply(count, list(province, category), sum, default = 0))

# Calculate the Shannon-Weaver Diversity Index
diversity_indices <- apply(species_matrix, 1, function(x) diversity(x))
diversity_df <- data.frame(province = names(diversity_indices), Shannon_Index = diversity_indices)


##############################################################
###PART 3 figure out the number of sppsites or spp per PROVINCE for top part of plot
##############################################################

#ggplot code for counts of seabird species per ecosystem

#set up data
province_site_count <- prey.df %>%
  group_by(province) %>%
  summarise(sppCount = n_distinct(spp)) #to assess diversity of spp seabirds in each PROVINCE

#plot to check.
ggplot(province_site_count, aes(x = province, y = sppCount, fill = province)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Count of Distinct Seabird Species in each Ecosystem",
       x = "Province",
       y = "Seabird Species") +
  theme(legend.position = "none")

##############################################################
###Combine plots into a double plot.
##############################################################

# set a new order for the X-axis that orders from the greatest prey diversity to lowest
new_order <- c("Cold Temperate Northeast Pacific",
               "Cold Temperate Northwest Pacific",
               "Arctic", 
               "Cold Temperate Northwest Atlantic", 
               "Northern European Seas")

prey.df$province <- factor(prey.df$province, levels = new_order)
province_site_count$province <- factor(province_site_count$province, levels = new_order)


#modify y-axis in sppsite_plot to come down from top
spp_plot <- ggplot(province_site_count, aes(x = province, y = sppCount)) +
  geom_bar(stat = "identity", width = 0.25) +
  theme_minimal(base_size = 15) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Horizontal line at y = 0
  geom_vline(xintercept = 0, color = "black", size = 0.5) + # Vertical line at the start of x-axis
  scale_y_reverse() + 
  labs(title = "",
       x = "",
       y = "Seabird Species") +
  theme(legend.position = "none", 
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),  # Remove major grid lines on x-axis
      panel.grid.minor = element_blank()) # Keep major grid lines on y-axis only
spp_plot # check to see if worked

## set some parameters to adjust arrow and text annotations
y_position_for_arrow <- 120
mid_x_position <- (1 + length(levels(prey.df$province))) / 2 

final_prey_plot <- annotated_plot +  
  annotate("segment", x = 1, xend = length(levels(prey.df$province)),
                                              y = y_position_for_arrow, yend = y_position_for_arrow, 
                                              arrow = arrow(type = "closed", length = unit(0.5, "cm")),
                                              colour = "black", size = 2.5)+
  annotate("label", x = mid_x_position, y = y_position_for_arrow + 0.5, 
           label = "Decreasing prey diversity", size = 8, color = "black")

#combine the two plots
combined_plot <- grid.arrange(spp_plot, final_prey_plot, ncol = 1, heights = c(.35, 1))

#save it
ggsave(filename = "figures/raw_prey_counts_annotated.png", 
       plot = combined_plot, 
       width = 9, height = 11, 
       dpi = 300)



####################################################
#create plot for second figure (proportion prey used)
####################################################

#define order for 2nd plot
prey.df2$variable <- factor(prey.df2$variable, levels = all_species)

# Define species groups for plotting
species_color_map <- c("Clupeidae" = "#41ab5dff",  "Sardinops" = "#41ab5dff", "Scomberidae" = "#41ab5dff", "Engraulidae" ="#41ab5dff", "Osmeridae" = "#41ab5dff", "Scomberesocidae" = "#41ab5dff", "Mallotus" = "#41ab5dff", "Stromateidae" = "#41ab5dff", "Ammodytidae" = "#41ab5dff", "Cottidae" = "#08519cff", "Hexagrammidae" = "#08519cff", "Pholidae" = "#08519cff", "Pleuronectiformes" = "#08519cff","Gadidae" = "#08519cff", "Paralichthyidae" = "#08519cff", "Scorpaenidae" = "#08519cff", "Myctophidae" = "#8573cbff", "Copepoda" = "#de2d26ff", "Amphipoda" = "#de2d26ff", "Euphausiidae" = "#de2d26ff", "Cephalopoda" = "#fec44f", "Aves" = "#bdbdbd", "Variable" = "#bdbdbd", "Gelatinous" = "#bdbdbd", "Kleptoparasitism" = "#bdbdbd", "Polychaeta" = "#bdbdbd", "Unid Fish" = "#bdbdbd", "Unid Invertebrate" = "#bdbdbd", "Zooplankton" = "#bdbdbd", "Polyplacophora" = "#bdbdbd", "Crustacea" = "#bdbdbd")

#order the vector
all_species <- c("Aves", "Variable", "Gelatinous", "Kleptoparasitism", "Polychaeta", "Unid Fish", "Unid Invertebrate", "Zooplankton", "Polyplacophora", "Crustacea", "Cephalopoda", "Copepoda", "Amphipoda", "Euphausiidae", "Myctophidae", "Cottidae", "Hexagrammidae", "Pholidae", "Pleuronectiformes","Gadidae", "Paralichthyidae", "Scorpaenidae", "Clupeidae",  "Sardinops", "Scomberidae", "Engraulidae", "Osmeridae", "Scomberesocidae", "Mallotus", "Stromateidae", "Ammodytidae")

#order PROVINCE by decreasing Diverstiy index (e.g. Fig 1)
prey.df2$province <- factor(prey.df2$province, levels = new_order)
#order prey categoried by code and color
prey.df2$variable <- factor(prey.df2$variable, levels = all_species)

preyuseplot <- ggplot(prey.df2) + 
  geom_col(aes(x = province, y = value, fill = variable), color = "white") +  # Add white lines between stacks
  scale_fill_manual(values = species_color_map) + 
  scale_x_discrete(labels = new_labels) +
  labs(x = "Ecosystem (in order of declining prey diversity)", y = "Proportion of prey used by birds", fill = "Prey item") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "none")
preyuseplot

#set adjustments for manual legend
x_start <- 6.75  # Adjust as needed
t_start <- 5.6
y_start <- .85   # Starting point on the y-axis
y_step <- .1     # Space between legend items
legend_categories <- c("Other", "Squid", "Crustacean \nzooplankton","Mesopelagic fish","Pelagic schooling \nfish","Nearshore and \ndemersal fish")

# Add colored boxes and text for each category
for(i in 1:length(legend_categories)) {
  y_position <- y_start - (i * y_step)
  preyuseplot <- preyuseplot +
    annotate("rect", xmin = x_start, xmax = x_start, ymin = y_position, ymax = y_position + .1, 
            fill = legend_colors[i]) +
    annotate("rect", xmin = t_start, xmax = t_start +0.2, ymin = y_position, ymax = y_position + .1, 
            fill = legend_colors[i]) +
    annotate("text", x = t_start+.25, y = y_position+.05, label = legend_categories[i],
             hjust = 0, size = 4)  #
}
preyuseplot

diversity_df$province <- factor(diversity_df$province, levels = new_order)

for(i in 1:length(diversity_df$province)) {
  preyuseplot <- preyuseplot +
    annotate("text", x = diversity_df$province[i], y = 1.1, label = round(diversity_df$Shannon_Index[i], 2),
             hjust = 0.5, size = 8) + 
    theme(axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) 
}
preyuseplot 

#combine the two plots
ggsave("figures/proportional_prey_use.png", plot = preyuseplot, dpi = 300, width = 10, height = 8, units = "in")


####################################################
#create some tables to go with figures.
####################################################


#add a column for Cateogy grouping that mimic the figure
prey_table  <- prey.df  %>% mutate(Category=case_when(
  adjusted_code=="other"~"Other",
  adjusted_code=="unid fish"~"Other",
  adjusted_code=="birds"~"Other",
  adjusted_code=="parasitism"~"Other",
  adjusted_code=="invertebrate"~"Other",
  adjusted_code=="crustacean"~"Other",
  adjusted_code=="polychaete"~"Other",
  adjusted_code=="cephalopoda"~"Squid",
  adjusted_code=="euphaucidae"~"Crustacean zooplankton",
  adjusted_code=="copepoda"~"Crustacean zooplankton",
  adjusted_code=="amphipoda"~"Crustacean zooplankton",
  adjusted_code=="ammodytes"~"Ammodytes",
  adjusted_code=="myctophidae"~"Mesopelagic fish",
  adjusted_code=="scomberesocidae"~"Mesopelagic fish",
  adjusted_code=="Myctophidae"~"Mesopelagic fish",
  adjusted_code=="clupeidae"~"Pelagic schooling fish",
  adjusted_code== "engraulis"~"Pelagic schooling fish",
  adjusted_code== "stromateidae"~"Pelagic schooling fish",
  adjusted_code=="mallotus"~"Nearshore fish",
  adjusted_code=="sebastes"~"Nearshore fish",
  adjusted_code=="gadidae"~"Demersal fish",
  adjusted_code=="cottidae"~"Demersal fish",
  adjusted_code=="pleuronectidae"~"Demersal fish",
  adjusted_code=="merluccius"~"Demersal fish",
  adjusted_code=="hexagrammidae"~"Demersal fish",
  adjusted_code=="pholidae"~"Demersal fish",
  TRUE~adjusted_code))

# Define the order for Category
category_order_1 <- c("Other", "Squid", "Crustacean zooplankton", "Ammodytes", 
                    "Mesopelagic fish", "Pelagic schooling fish", "Nearshore fish", 
                    "Demersal fish")

# Group by PROVINCE and Category
Table1 <- prey_table %>%
  group_by(PROVINCE, Category) %>%
  mutate(Category = factor(Category, levels = category_order_1)) %>%
  summarise(prey_count = n(), seabird_count = n_distinct(spp), sppsite_count = n_distinct(sppsite)) %>%
  arrange(PROVINCE, Category)  # Arrange by PROVINCE and Category in the specified order

# rearrange columns
Table <- Table %>%
  dplyr::select(PROVINCE, Category, prey_count, seabird_count, sppsite_count)

# Convert data frame to a flextable
ft <- flextable(Table)

# Create a Word document
doc <- read_docx()

# Add the flextable to the Word document
doc <- doc %>% 
  body_add_flextable(ft)

# Save the Word document
file_path <- "Table1.docx"  # Change the path and filename as needed
print(doc, target = file_path)

####Table 2
# Define the order for Category
code_order_1 <- c("cottidae", "hexagrammidae", "pholidae", "pleuronectidae","gadidae","merluccius",
                  "mallotus", "sebastes","clupeidae",  "engraulis", "stromateidae",
                  "myctophidae","scomberesocidae","ammodytes",
                  "copepoda", "amphipoda", "crustacean", "euphaucidae",
                  "cephalopoda",
                  "birds", "invertebrate", "other", "parasitism", "polychaete"
)
Table2 <- prey.df%>%
  group_by(PROVINCE,adjusted_code) %>%
  mutate(code = factor(adjusted_code, levels = code_order_1)) %>%
  summarise(prey_count = n(), seabird_count = n_distinct(spp), sppsite_count = n_distinct(sppsite)) %>%
  arrange(PROVINCE, adjusted_code)  # Arrange by PROVINCE and Category in the specified order

Table2 <- Table2 %>%
  dplyr::select(PROVINCE, adjusted_code, prey_count, seabird_count, sppsite_count)

# Convert data frame to a flextable
ft <- flextable(Table2)

# Create a Word document
doc <- read_docx()

# Add the flextable to the Word document
doc <- doc %>% 
  body_add_flextable(ft)

# Save the Word document
file_path <- "Table2.docx"  # Change the path and filename as needed
print(doc, target = file_path)
