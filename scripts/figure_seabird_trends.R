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
#set up ecosystem labels for facets
ecosystem.labs <- c("Arctic", "Cold Northeast Pacific", "Northwest Atlantic", "Northwest Pacific", "Hawaii", "Northern European Seas", "Warm Northeast Pacific")
names(ecosystem.labs) <- c("Arctic", "Cold Temperate Northeast Pacific", "Cold Temperate Northwest Atlantic", "Cold Temperate Northwest Pacific", "Hawaii", "Northern European Seas", "Warm Temperate Northeast Pacific")
#====

#PLOT BREEDING SUCCESS TRENDS
#====
#generate model simulated data
ff <- pltmm(n.trends, dat)
eco <- as.vector(unique(dat$PROVINCE))#list of ecosystems

#Limit predicted value domain to the duration of time series
temp <- list()
for(i in 1:7){
  temp[[i]] <- filter(ff, PROVINCE == eco[i], yearno>min(filter(dat, PROVINCE == eco[i])$yearno)-0.1, yearno<max(filter(dat, PROVINCE == eco[i])$yearno)+0.1)
}
ff <- bind_rows(temp)

#plot trends for each ecosystem and seabird time series
plot <- ggplot(data = dat, aes(x = yearno + min(dat$year), y = stbs, group = sppsite)) +
  geom_smooth(data = dat, aes(x = yearno + min(year), y = stbs, group = sppsite, colour = PROVINCE), linetype = "dashed",
              method = "lm", se = FALSE,
              lwd = .5) +
  geom_ribbon(data = ff, aes(x = yearno + min(dat$year), ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.45, inherit.aes = FALSE) +
  geom_line(data = ff, aes(x = yearno + min(dat$year), y = y, colour = PROVINCE), lwd = 1, inherit.aes = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  facet_wrap(~PROVINCE, ncol = 3, labeller = labeller(PROVINCE = ecosystem.labs)) +
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
  scale_color_manual(values=values)
ggsave(filename = "figures/seabird.trends.jpg", plot = plot, width = 9, height = 6)#save plot
#====

#PLOT TIME SERIES PER COLONY WITH STANDARD ERROR
#====
#generate a plot showing seabird breeding success grouped by site (colony) with error; figure included as supplemental Figure S3. 
ggplot(filter(dat), aes(year, bs, color = spp)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(vars(PROVINCE ,site), ncol = 4) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "Breeding Success") + 
  guides(color = "none")
ggsave("figures/supplementary/raw_sppsite_trends.jpg", width = 15, height = 20)
#====