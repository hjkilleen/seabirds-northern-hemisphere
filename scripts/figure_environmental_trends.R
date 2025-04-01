#Script to generate Figure 2 in published paper, visualizing linear trends in modeled estimates of mixed layer temperature and stratification across seven northern hemisphere ecosystems. 
#Written by Helen Killeen

# Tue Sep 17 15:21:49 2024 ------------------------------

#SOURCES
#====
source("scripts/0_Seabird_Helpers.R")

#load data for supplement
df.pb <- readRDS("data/df.pb.rds")#pre-breeding season averaged data
df.b <- readRDS("data/df.b.rds")#breeding season averaged data

#load fitted models
pb.temp.trends <- readRDS("output/pb.temp.trends.rds")
pb.strat.trends <- readRDS("output/pb.strat.trends.rds")
b.temp.trends <- readRDS("output/b.temp.trends.rds")
b.strat.trends <- readRDS("output/b.strat.trends.rds")
#====

#COLOR PALETTE
#====
#Create color palette
cols <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#7cdc54", "#fcdc54", "#cc9c44")
#====

#PLOTTING ENVIRONMENT TRENDS
#====
#generate simulated data for plotting
ff.pt <- pltmm(pb.temp.trends, df.pb)
ff.ps <- pltmm(pb.strat.trends, df.pb)
ff.bt <- pltmm(b.temp.trends, filter(df.b, year<2020))#filter out the one Hawaii year at end of time series due to breeding season spanning calendar year change
ff.bs <- pltmm(b.strat.trends, filter(df.b, year<2020))

#Plot of seasonally stratified temperature and stratification trends by ecosystem
plot <- ggplot() + 
  geom_ribbon(data = ff.pt, aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi), fill = "#fc0000", alpha = 0.15) + 
  geom_line(data = ff.pt, aes(x = yearno + 1993, y = y), linewidth = 1, color = "#fc0000", linetype = "dashed") + 
  geom_ribbon(data = ff.bt, aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi), fill = "#fc0000", alpha = 0.15) + 
  geom_line(data = ff.bt, aes(x = yearno + 1993, y = y), linewidth = 1, color = "#fc0000") + 
  geom_ribbon(data = ff.ps, aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi), fill = "#0000fc", alpha = 0.15) + 
  geom_line(data = ff.ps, aes(x = yearno + 1993, y = y), linewidth = 1, color = "#0000fc", linetype = "dashed") + 
  geom_ribbon(data = ff.bs, aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi), fill = "#0000fc", alpha = 0.15) + 
  geom_line(data = ff.bs, aes(x = yearno + 1993, y = y), linewidth = 1, color = "#0000fc") + 
  geom_hline(data = ff.bs, aes(yintercept = 0), linetype = "dotted", color = "black") + 
  labs(x = "Year", y = "Standardized Value") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 10,family="Helvetica"), 
    axis.title = element_text(size = 10,family="Helvetica"),
    axis.title.x = element_text(vjust = -2,family="Helvetica",size=12),
    axis.title.y = element_text(vjust = 2,family="Helvetica",size=12), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="none")+
  facet_wrap(~PROVINCE)

#SAVE ENVIRONMENTAL TREND PLOT
#====
ggsave(filename = "figures/env.trends.jpg", plot = plot, width = 9, height = 6)#save plot
#====

#GENERATE SUPPLEMENTAL FIGURES SHOWING TRENDS WITHIN EACH ECOREGION
#====
#generate a plot showing mixed layer temperature grouped by ecosystem with error; figure included as supplemental Figure S1. 
pts <- ggplot(df.pb, aes(year, st.temp, color = ECOREGION)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(vars(PROVINCE), ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "Standardized Mixed Layer Temperature (°C)", title = "Pre-Breeding Season") + 
  guides(color = "none")
bts <- ggplot(df.b, aes(year, st.temp, color = ECOREGION)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(vars(PROVINCE), ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "", title = "Breeding Season") + 
  guides(color = "none")
ggarrange(pts, bts, ncol = 2)
ggsave("figures/supplementary/standardized_mixed_layer_temp_trends.jpg", width = 10, height = 15)

#generate a plot showing water column stratification grouped by ecosystem with error; figure included as supplemental Figure S2. 
pss <- ggplot(df.pb, aes(year, st.strat, color = ECOREGION)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(vars(PROVINCE), ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "Standardized Water Column Stratification (PEA)", title = "Pre-Breeding Season") + 
  guides(color = "none")
bss <- ggplot(df.b, aes(year, st.strat, color = ECOREGION)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(vars(PROVINCE), ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "", title = "Breeding Season") + 
  guides(color = "none")
ggarrange(pss, bss, ncol = 2)
ggsave("figures/supplementary/standardized_water_column_strat_trends.jpg", width = 10, height = 15)
#====