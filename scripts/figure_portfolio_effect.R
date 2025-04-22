#Script to generate prey diversity vs trend estimate linear model plot for manuscript. 

#Written by Helen Killeen

# Wed Apr  2 10:27:11 2025 ------------------------------

#SOURCES & DATA
#====
source("scripts/0_Seabird_Helpers.R")

res <- readRDS("output/trend_diversity_estimates.rds")#read in model results from 4_GLMM_Biophysical
#====

#SET UP
#====
cols <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#fcdc54")
driver.labs <- c("Time Series", "Temperature", "Stratification")
names(driver.labs) <- c("time", "temp", "strat")
res$driver <- factor(res$driver, levels = c("time", "temp", "strat"))

#create x axis labels
new_labels <- c("Cold Temperate Northeast Pacific" = "Cold\nNortheast Pacific",
                "Cold Temperate Northwest Atlantic" = "Northwest Atlantic",
                "Cold Temperate Northwest Pacific" = "Northwest Pacific",
                "Northern European Seas" = "Northern European\nSeas",
                "Warm Temperate Northeast Pacific" = "Warm\nNortheast Pacific")
#====

#GENERATE PLOT
#====
plot <- ggplot() + 
  geom_hline(data = res[complete.cases(res),], aes(yintercept = 0), linetype = "dotted", color = "black") + 
  geom_point(data = res[complete.cases(res),], aes(Shannon_Index, trend, color = PROVINCE, shape = season), size = 6) + 
  geom_smooth(data = res[complete.cases(res),], aes(Shannon_Index, trend), method = "lm", color = "black") +
  facet_wrap(~driver, labeller = labeller(driver = driver.labs)) + 
  labs(x = "Shannon-Wiener Index (H')", y = "Trend Estimate", color = "Ecosystem") + 
  scale_shape_discrete(name = "Model", labels = c("Breeding Season", "Pre-Breeding Season", "Time Series")) + 
  scale_color_manual(values = cols, labels = new_labels) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 10,family="Helvetica"), 
    axis.title = element_text(size = 10,family="Helvetica"),
    axis.title.x = element_text(vjust = -2,family="Helvetica",size=12),
    axis.title.y = element_text(vjust = 2,family="Helvetica",size=12), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
  #guides(color = "none")

ggsave(filename = "figures/prey.response.jpg", plot = plot, width = 9, height = 5)#save plot
  
#Extract model results for use in manuscript text
tab_model(lm(trend~Shannon_Index, data = filter(res, driver == "time")), show.ci = .9)#S31.1
tab_model(lm(trend~Shannon_Index, data = filter(res, driver == "temp")), show.ci = .9)#S31.2
tab_model(lm(trend~Shannon_Index, data = filter(res, driver == "strat")), show.ci = .9)#S31.3
#====