#Script to generate prey profile figures for each seabird species by ecosystem. 
#Written by Helen Killeen & Brian Hoover

# Fri Sep 20 15:00:39 2024 ------------------------------

#SOURCES & DATA
#====
source("scripts/0_Seabird_Helpers.R")

prey.df <- readRDS("data/prey.df.rds")#load cleaned prey records
prey.df2 <- readRDS("data/prey.df.proportional.rds")
#====

#PLOT RAW PREY COUNTS PER ECOSYSTEM (SUPPLEMENTAL FIGURE S6)
#===============
# Define species groups for plotting using color palette
green_species <- c("Clupeidae",  "Sardinops", "Scomberidae", "Engraulidae", "Osmeridae", "Scomberesocidae", "Stromateidae", "Ammodytidae")#Pelagic schooling fish
blue_species <- c("Cottidae", "Hexagrammidae", "Pholidae", "Pleuronectiformes","Gadidae", "Paralichthyidae", "Scorpaenidae")#Nearshore and demersal fish
purple_species <- c("Myctophidae")#Myctophids
red_species <- c("Copepoda", "Amphipoda", "Euphausiidae")#Macrozooplankton
yellow_species <- c("Cephalopoda")#Squid
gray_species <- c("Aves", "Variable", "Gelatinous", "Kleptoparasitism", "Polychaeta", "Unid Fish", "Unid Invertebrate", "Zooplankton", "Polyplacophora", "Crustacea")#Other

# Create color vectors for each group of species
# Adjust these colors to preference
green_colors <- colorRampPalette(c("#edf8e9ff","#41ab5dff"))(length(green_species))
blue_colors <- colorRampPalette(c("#eff3ffff","#08519cff"))(length(blue_species))   
purple_colors <- colorRampPalette(c("#8573cbff","#8573cbff"))(length(purple_species))  # Single color for purple
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
legend_categories <- c("PREY GROUP", "Other", "Squid", "Crustacean zooplankton","Mesopelagic fish","Pelagic schooling fish","Nearshore and demersal fish")
legend_colors <- c("white", "#bdbdbd","#fec44f","#de2d26ff","#8573cbff","#41ab5dff","#08519cff")

#prepare an annotated plot
annotated_plot <- original_plot

#set adjustments
x_start <- 2.65 # Starting point on the x-axis
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

#save figure S6
ggsave("figures/supplementary/raw_prey_counts.jpg", plot = annotated_plot, dpi = 300, width = 10, height = 8, units = "in")
#==============


#PLOT PREY PORTFOLIO PER ECOSYSTEM FOR MAIN TEXT
#===============
#Count the number of sppsites or spp per ecosystem for top part of plot
province_site_count <- prey.df %>%
  group_by(province) %>%
  summarise(sppCount = n_distinct(spp)) #to assess diversity of spp seabirds in each ecosystem

# Define species groups for plotting
species_color_map <- c("Clupeidae" = "#41ab5dff",  "Sardinops" = "#41ab5dff", "Scomberidae" = "#41ab5dff", "Engraulidae" ="#41ab5dff", "Osmeridae" = "#41ab5dff", "Scomberesocidae" = "#41ab5dff", "Stromateidae" = "#41ab5dff", "Ammodytidae" = "#41ab5dff", "Cottidae" = "#08519cff", "Hexagrammidae" = "#08519cff", "Pholidae" = "#08519cff", "Pleuronectiformes" = "#08519cff","Gadidae" = "#08519cff", "Paralichthyidae" = "#08519cff", "Scorpaenidae" = "#08519cff", "Myctophidae" = "#8573cbff", "Copepoda" = "#de2d26ff", "Amphipoda" = "#de2d26ff", "Euphausiidae" = "#de2d26ff", "Cephalopoda" = "#fec44f", "Aves" = "#bdbdbd", "Variable" = "#bdbdbd", "Gelatinous" = "#bdbdbd", "Kleptoparasitism" = "#bdbdbd", "Polychaeta" = "#bdbdbd", "Unid Fish" = "#bdbdbd", "Unid Invertebrate" = "#bdbdbd", "Zooplankton" = "#bdbdbd", "Polyplacophora" = "#bdbdbd", "Crustacea" = "#bdbdbd")

#order the vector
all_species <- c("Aves", "Variable", "Gelatinous", "Kleptoparasitism", "Polychaeta", "Unid Fish", "Unid Invertebrate", "Zooplankton", "Polyplacophora", "Crustacea", "Cephalopoda", "Copepoda", "Amphipoda", "Euphausiidae", "Myctophidae", "Cottidae", "Hexagrammidae", "Pholidae", "Pleuronectiformes","Gadidae", "Paralichthyidae", "Scorpaenidae", "Clupeidae",  "Sardinops", "Scomberidae", "Engraulidae", "Osmeridae", "Scomberesocidae", "Stromateidae", "Ammodytidae")

new_order <- c("Northern European Seas", "Cold Temperate Northwest Atlantic", "Arctic", "Cold Temperate Northwest Pacific", "Cold Temperate Northeast Pacific")#set new order for the ecosystems based on high to low prey portfolio diversity (see scripts/5_Prey_Portfolio_Diversity)
prey.df2 <-filter(prey.df2, !province %in% c("Mediterranean Sea", "Hawaii", "Warm Temperate Northeast Pacific"))#filter out un-needed provinces
prey.df2$province <- factor(prey.df2$province, levels = new_order)
#order prey categorized by code and color
prey.df2$variable <- factor(prey.df2$variable, levels = all_species)

preyuseplot <- ggplot(prey.df2) + #plot initial version
  geom_col(aes(x = province, y = value, fill = variable), color = "white") +  # Add white lines between stacks
  scale_fill_manual(values = species_color_map) + 
  scale_x_discrete(labels = new_labels) +
  labs(x = "Ecosystem (in order of increasing prey diversity)", y = "Proportion of prey used by birds", fill = "Prey item") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust=.5), 
        legend.position = "none")
preyuseplot#check

#set adjustments for manual legend
x_start <- 6.75  # Adjust as needed
t_start <- 5.6
y_start <- .85   # Starting point on the y-axis
y_step <- .1     # Space between legend items
legend_categories <- c("Other", "Squid", "Crustacean \nzooplankton","Mesopelagic fish","Pelagic schooling \nfish","Nearshore and \ndemersal fish")

legend_colors <- c("#bdbdbd","#fec44f","#de2d26ff","#8573cbff","#41ab5dff","#08519cff")#reset legend color map

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

diversity_df$province <- factor(diversity_df$province, levels = new_order)#order Shannon-Wiener indices

for(i in 1:length(diversity_df$province)) {
  preyuseplot <- preyuseplot +
    annotate("text", x = diversity_df$province[i], y = 1.1, label = paste("H'=",round(diversity_df$Shannon_Index[i], 2), sep=""),
             hjust = 0.5, size = 6) + 
    theme(axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) 
}

for(i in 1:length(province_site_count$province)) {
  preyuseplot <- preyuseplot +
    annotate("text", x = province_site_count$province[i], y = 1.05, label = paste("n spp=",province_site_count$sppCount[i], sep=""),
             hjust = 0.5, size = 6) + 
    theme(axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) 
}
preyuseplot 

#combine the two plots and save Figure 4
ggsave("figures/proportional_prey_use.png", plot = preyuseplot, dpi = 300, width = 10, height = 8, units = "in")
#=====
