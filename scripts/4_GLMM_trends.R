#Frequentist modeling of seabird breeding success trends by ecosystem
#Helen Killeen, adapted from code written by Dave Shoeman

# Thu Aug 17 13:53:55 2023 ------------------------------

#LIBRARIES & SOURCES
#====
source("scripts/0_Seabird_Helpers.R")
dat <- readRDS("data/dat.rda")
library(ggpubr)
library(emmeans)
library(sjPlot)
#=====

#SET UP ENVIRONMENTAL TRENDS
#====
dat <- filter(dat, lat>0)#limit to N hemisphere

#drop the Med
dat <- dat %>% mutate_if(is.character, as.factor)#convert new variables to factors
dat <- droplevels(filter(dat, PROVINCE != "Mediterranean Sea"))#get rid of Med

region_list <- unique(dplyr::select(dat, ECOREGION, PROVINCE))#get full list of sppsites
region_list$file <- paste("data/ecoregion_environment/", gsub("/", "", gsub(" ", "_", region_list$ECOREGION)), "_MEOW.csv", sep = "")#add a file ID to link with environmental data

dat_files <- list.files("data/ecoregion_environment/")#get list of GLORYS-12 environmental data files 

out1 <- list()
for(i in 1:length(dat_files)){
  #Filter datasets to relevant sppsites and years
  x <- read_csv(paste("data/ecoregion_environment/", dat_files[i], sep = ""))#read in temperature data
  x$year <- year(x$time)#extract year
  x$month <- month(x$time)#extract month
  x$file <- rep(paste("data/ecoregion_environment/", dat_files[i], sep = ""), nrow(x))
  y <- left_join(x, region_list)
  out1[[i]] <- y
}
df1 <- bind_rows(out1)
df1 <- filter(df1, PROVINCE != "NA")

#Create new dataframe with only PRE-BREEDING season data
df.pb <- df1 %>% 
  filter(case_when(PROVINCE == "Arctic" ~ month >= 3 & month <= 5,
                   PROVINCE == "Cold Temperate Northeast Pacific" ~ month >= 3 & month <= 5,
                   PROVINCE == "Cold Temperate Northwest Pacific" ~ month >= 3 & month <= 5,
                   PROVINCE == "Cold Temperate Northwest Atlantic" ~ month >= 3 & month <= 5,
                   PROVINCE == "Warm Temperate Northeast Pacific" ~ month >= 1 & month <= 2,
                   PROVINCE == "Hawaii" ~ month >= 8 & month <= 11,
                   PROVINCE == "Northern European Seas" ~ month >= 2 & month <= 5,)) %>% 
  group_by(year, PROVINCE, ECOREGION) %>% 
  summarise(temp = mean(thetao), strat = mean(PEA), chl = mean(chl)) %>% 
  as.data.frame()#compute seasonal mean values per ecoregion
out <- list() 
for(i in levels(df.pb$ECOREGION)) {#compute scales values
  d <- filter(df.pb, ECOREGION == i) %>% # For each time series
    mutate(st.temp = scale(temp), # Compute scaled temperature
           st.strat = scale(strat), # Compute scaled stratification
           st.chl = scale(chl)) # Compute scaled chlorophyll
  out[[i]] <- d
}
df.pb <- bind_rows(out) # Bind the elements of the list back into a data frame
df.pb$yearno <- df.pb$year-min(df.pb$year)#add year index

#Create new dataframe with only BREEDING season data
df.b <- df1 %>% 
  filter(case_when(PROVINCE == "Arctic"~ month >= 6 & month < 9,
                   PROVINCE == "Cold Temperate Northeast Pacific" ~ month >= 6 & month <= 9,
                   PROVINCE == "Cold Temperate Northwest Pacific" ~ month >= 6 & month <= 8,
                   PROVINCE == "Cold Temperate Northwest Atlantic" ~ month >= 6 & month <= 8,
                   PROVINCE == "Warm Temperate Northeast Pacific" ~ month >= 3 & month <= 7,
                   PROVINCE == "Hawaii" ~ month %in% c(1:5, 12),
                   PROVINCE == "Northern European Seas" ~ month >= 5 & month <= 8,))

df.b.h <- filter(df.b, PROVINCE == "Hawaii")#group Hawaii December with the following year
for(j in 1:nrow(df.b.h)){
  if(df.b.h$month[j]>10){
    df.b.h$year[j] <- df.b.h$year[j]+1
  }
}
df.b <- rbind(filter(df.b, PROVINCE != "Hawaii"), df.b.h)#recombine dataframe with Hawaii edit

df.b <- df.b %>% 
  group_by(year, PROVINCE, ECOREGION) %>% 
  summarise(temp = mean(thetao), strat = mean(PEA), chl = mean(chl)) %>% 
  as.data.frame()#compute seasonal mean values per ecoregion
out <- list() 
for(i in levels(df.b$ECOREGION)) {#compute scales values
  d <- filter(df.b, ECOREGION == i) %>% # For each time series
    mutate(st.temp = scale(temp), # Compute scaled temperature
           st.strat = scale(strat), # Compute scaled stratification
           st.chl = scale(chl)) # Compute scaled chlorophyll
  out[[i]] <- d
}
df.b <- bind_rows(out) # Bind the elements of the list back into a data frame
df.b$yearno <- df.b$year-min(df.b$year)#add year index

#Create color palette
cols <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#7cdc54", "#fcdc54", "#cc9c44")
#====

#MODEL ENVIRONMENTAL TRENDS
#====
#Pre-Breeding Season
#temperature
pb.temp.trends <- lme(st.temp ~ yearno + PROVINCE + yearno:PROVINCE,
                random = ~yearno|ECOREGION, # Allow random slopes by time series
                control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                method = "ML",
                data = df.pb)
saveRDS(pb.temp.trends, file = "output/pb.temp.trends.rds")#save model
#stratification
pb.strat.trends <- lme(st.strat ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|ECOREGION, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = df.pb)
saveRDS(pb.strat.trends, file = "output/pb.strat.trends.rds")#save model
#chlorophyll
pb.chl.trends <- lme(st.chl ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|ECOREGION, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = df.pb)
saveRDS(pb.chl.trends, file = "output/pb.chl.trends.rds")#save model

#full model summary
tab_model(pb.temp.trends)
tab_model(pb.strat.trends)
tab_model(pb.chl.trends)

#provincial effects
emtrends(pb.temp.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends
emtrends(pb.strat.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends
emtrends(pb.chl.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends

#Breeding Season
#temperature
b.temp.trends <- lme(st.temp ~ yearno + PROVINCE + yearno:PROVINCE,
                      random = ~yearno|ECOREGION, # Allow random slopes by time series
                      control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                      method = "ML",
                      data = df.b)
saveRDS(b.temp.trends, file = "output/b.temp.trends.rds")#save model
#stratification
b.strat.trends <- lme(st.strat ~ yearno + PROVINCE + yearno:PROVINCE,
                       random = ~yearno|ECOREGION, # Allow random slopes by time series
                       control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                       method = "ML",
                       data = df.b)
saveRDS(b.strat.trends, file = "output/b.strat.trends.rds")#save model
#chlorophyll
b.chl.trends <- lme(st.chl ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|ECOREGION, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = df.b)
saveRDS(b.chl.trends, file = "output/b.chl.trends.rds")#save model

#full model summary
tab_model(b.temp.trends)
tab_model(b.strat.trends)
tab_model(b.chl.trends)

#provincial effects
emtrends(b.temp.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends
emtrends(b.strat.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends
emtrends(b.chl.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends
#====

#PLOTTING ENVIRONMENT TRENDS
#====
#generate simulated data for plotting
ff.pt <- pltmm(pb.temp.trends, df.pb)
ff.ps <- pltmm(pb.strat.trends, df.pb)
ff.pc <- pltmm(pb.chl.trends, df.b)
ff.bt <- pltmm(b.temp.trends, df.b)
ff.bs <- pltmm(b.strat.trends, df.b)
ff.bc <- pltmm(b.chl.trends, df.b)

#generate subplots
pt <- ggplot(data = ff.pt) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), size = 1) + 
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
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), size = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "Scaled Stratification (PEA)") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

pc <- ggplot(data = ff.pc) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), size = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "Scaled Mixed Layer Chlorophyll Content") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

bt <- ggplot(data = ff.bt) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), size = 1) + 
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
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), size = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

bc <- ggplot(data = ff.bc) + 
  geom_ribbon(aes(x = yearno + 1993, ymin = se.lw, ymax = se.hi, fill = PROVINCE), alpha = 0.15) + 
  geom_line(aes(x = yearno + 1993, y = y, color = PROVINCE), size = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  labs(x = "Year", y = "") + 
  lims(y = c(-1.5,1.5)) + 
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) + 
  theme_classic() + 
  theme(text = element_text(size = 15, family = "Helvetica"),
        legend.position = "none", plot.margin = unit(c(0,.5,0,.5), "cm"))

#MERGE & SAVE ENVIRONMENTAL TREND PLOT
#====
#plot <- ggarrange(pt, bt, ps, bs, pc, bc, ncol = 2, nrow = 3)
plot <- ggarrange(pt, bt, ps, bs, ncol = 2, nrow = 2)#removed chlorophyll
ggsave(filename = "figures/env.trends.jpg", plot = plot, width = 12, height = 7)
#====


#GENERATE SUPPLEMENTARY ENVIRONMENTAL TREND FIGURES
#====
x <-as.data.frame(ranef(b.strat.trends))
x <- arrange(x, yearno)
x$ECOREGION <- row.names(x)
x <- left_join(x, unique(dplyr::select(dat, ECOREGION, PROVINCE)))
x <- droplevels(x)
x$color <- rep(NA, nrow(x))

# Dotplot: Grouped Sorted and Colored
x$color[x$PROVINCE=="Arctic"] <- "#14c4ac"
x$color[x$PROVINCE=="Cold Temperate Northwest Atlantic"] <- "#8c54fc"
x$color[x$PROVINCE=="Cold Temperate Northeast Pacific"] <- "#fc944c"
x$color[x$PROVINCE=="Warm Temperate Northeast Pacific"] <- "#cc9c44"
x$color[x$PROVINCE=="Northern European Seas"] <- "#fcdc54"
x$color[x$PROVINCE=="Cold Temperate Northwest Pacific"] <- "#fc94e4"
x$color[x$PROVINCE=="Hawaii"] <- "#7cdc54"

dotchart(x$yearno,labels=x$ECOREGION,groups= x$PROVINCE,
         main="Ecoregion Level Stratification Trends",
         xlab="PEA/year", pt.cex = 2, pch = 16,gcolor="black", color=x$color)
abline(v =0)

saveRDS(n.trends, file = "output/n.trends.env.rds")#save model
#====

#MODEL PRIOVINCIAL BREEDING SUCCESS
#====
n.trends <- lme(stbs ~ yearno + PROVINCE + yearno:PROVINCE,
                     random = ~yearno|sppsite, # Allow random slopes by time series
                     correlation = corCAR1(form = ~ yearno|sppsite), # Incorporate temporal autocorrelation in discontinuous time series, by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     data = filter(dat, PROVINCE != "Hawaii"))

saveRDS(n.trends, file = "output/n.trends.rds")#save model

#full model summary
tab_model(n.trends)
#====

#PLOT BREEDING SUCCESS TRENDS
#====
values <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#7cdc54", "#fcdc54", "#cc9c44")

ff <- pltmm(n.trends, dat)#generate model simulated data

ggplot(data = dat, aes(x = yearno + min(dat$year), y = stbs, group = sppsite)) +
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
    legend.position="top",
    legend.text=element_text(size=10,family="Helvetica"),
    legend.title=element_text(size=10,family="Helvetica"))+
  scale_fill_manual(values=values)+
  scale_color_manual(values=values)+
  facet_wrap(vars(PROVINCE), ncol = 3)

x <-as.data.frame(ranef(n.trends))
x <- arrange(x, yearno)
x$sppsite <- row.names(x)
x <- left_join(x, unique(dplyr::select(dat, sppsite, PROVINCE)))
x <- droplevels(x)
x$color <- rep(NA, nrow(x))

# Dotplot: Grouped Sorted and Colored
x$color[x$PROVINCE=="Arctic"] <- "#14c4ac"
x$color[x$PROVINCE=="Cold Temperate Northwest Atlantic"] <- "#8c54fc"
x$color[x$PROVINCE=="Cold Temperate Northeast Pacific"] <- "#fc944c"
x$color[x$PROVINCE=="Warm Temperate Northeast Pacific"] <- "#cc9c44"
x$color[x$PROVINCE=="Northern European Seas"] <- "#fcdc54"
x$color[x$PROVINCE=="Cold Temperate Northwest Pacific"] <- "#fc94e4"
x$color[x$PROVINCE=="Hawaii"] <- "#7cdc54"

dotchart(x$yearno,labels=x$ECOREGION,groups= x$PROVINCE,
         main="Ecoregion Level Stratification Trends",
         xlab="PEA/year", pt.cex = 2, pch = 16,gcolor="black", color=x$color)
abline(v =0)
#====