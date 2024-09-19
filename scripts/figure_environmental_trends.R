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
ff.bt <- pltmm(b.temp.trends, df.b)
ff.bs <- pltmm(b.strat.trends, df.b)

#generate subplots
pt <- ggplot(data = ff.pt) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), linewidth = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "Scaled Mixed Layer Temperature", title = "Pre-Breeding Season") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

ps <- ggplot(data = ff.ps) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), linewidth = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "Scaled Stratification (PEA)") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

bt <- ggplot(data = ff.bt) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), linewidth = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "", title = "Breeding Season") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

bs <- ggplot(data = ff.bs) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), linewidth = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

#plot to extract legend
leg <- get_legend(ggplot(data = ff.bs) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), linewidth = 1) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "bottom", plot.margin = unit(c(0,.5,0,.5), "cm"),
        legend.title = element_blank()))

#MERGE & SAVE ENVIRONMENTAL TREND PLOT
#====
#plot <- ggarrange(pt, bt, ps, bs, pc, bc, ncol = 2, nrow = 3)
plot <- ggarrange(pt, bt, ps, bs, ncol = 2, nrow = 2)#combine subplots
plot_leg <- ggarrange(plot, leg, ncol = 1, nrow = 2, heights = c(4,1))#add legend
ggsave(filename = "figures/env.trends.jpg", plot = plot_leg, width = 12, height = 9)#save plot
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