#Script to generate Figure 2 in published paper, visualizing linear trends in modeled estimates of mixed layer temperature and stratification across seven northern hemisphere ecosystems. 
#Written by Helen Killeen

# Tue Sep 17 15:21:49 2024 ------------------------------

#SOURCES
#====
source("scripts/0_Seabird_Helpers.R")

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