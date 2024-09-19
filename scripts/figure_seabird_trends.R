#Script to generate Figure 3 in published paper, visualizing linear trends in modeled estimates of seabird breeding success across seven northern hemisphere ecosystems. 
#Written by Helen Killeen

# Wed Sep 18 06:45:25 2024 ------------------------------

#SOURCES
#====
source("scripts/0_Seabird_Helpers.R")

#load fitted model
n.trends <- readRDS("output/n.trends.rds")
#====

#COLOR PALETTE
#====
#Create color palette
values <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#7cdc54", "#fcdc54", "#cc9c44")
#====

#PLOT BREEDING SUCCESS TRENDS
#====
#generate model simulated data
ff <- pltmm(n.trends, dat)

#plot trends for each ecosystem and seabird time series
plot <- ggplot(data = dat, aes(x = yearno + min(dat$year), y = stbs, group = sppsite)) +
  geom_smooth(data = dat, aes(x = yearno + min(year), y = stbs, group = sppsite, colour = PROVINCE), linetype = "dashed",
              method = "lm", se = FALSE,
              lwd = 1) +
  geom_ribbon(data = ff, aes(x = yearno + min(dat$year), ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.45, inherit.aes = FALSE) +
  geom_line(data = ff, aes(x = yearno + min(dat$year), y = y, colour = PROVINCE), inherit.aes = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Year", 
       y = "Standardized breeding success \n  (± standard error)") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10,family="Helvetica"), 
    axis.title = element_text(size = 10,family="Helvetica"),
    axis.title.x = element_text(vjust = -2,family="Helvetica",size=12),
    axis.title.y = element_text(vjust = 2,family="Helvetica",size=12), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none")+
  scale_fill_manual(values=values)+
  scale_color_manual(values=values)+
  facet_wrap(vars(PROVINCE), ncol = 3)
ggsave(filename = "figures/seabird.trends.jpg", plot = plot, width = 9, height = 6)#save plot
#====

#PLOT TIME SERIES PER COLONY WITH STANDARD ERROR
#====
#generate a plot showing seabird breeding success grouped by site (colony) with error; figure included as supplemental Figure S3. 