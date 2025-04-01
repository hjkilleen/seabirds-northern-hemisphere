df.pb <- readRDS("data/df.pb.rds")
df.b <- readRDS("data/df.b.rds")
dat <- readRDS("data/dat.Rda")
n.trends <- readRDS("output/n.trends.rds")

df.pb <- left_join(df.pb, dat, by = c("year", "ECOREGION", "PROVINCE"))
df.b <- left_join(df.b, dat, by = c("year", "ECOREGION", "PROVINCE"))

#Pre-Breeding Season Temperature Model
pb.temp.stbs <- lme(stbs ~ st.temp + PROVINCE + st.temp:PROVINCE,
                random = ~st.temp|sppsite, # Allow random slopes by time series
                control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                method = "ML",
                na.action = na.omit,
                data = df.pb)
#Run graphical diagnostic plots
performance::check_model(pb.temp.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#Model results
tab_model(pb.temp.stbs, show.ci = .9)
tidy(confint(emtrends(pb.temp.stbs, pairwise ~ PROVINCE, var = "st.temp"), level = 0.9)$emtrends, conf.int  = TRUE) %>%
  kbl(caption = "Using 90% confidence interval", digits = 3) %>% 
  kable_classic()

#tab_model(pb.temp.stbs)
res.pb.temp <- tidy(emtrends(pb.temp.stbs, pairwise ~ PROVINCE, var = "st.temp")$emtrends, conf.int = TRUE)
res.pb.temp$season <- rep("pb", nrow(res.pb.temp))
res.pb.temp$driver <- rep("temp", nrow(res.pb.temp))
colnames(res.pb.temp)[2] <- "trend"

#Pre-Breeding Season Stratification Model
pb.strat.stbs <- lme(stbs ~ st.strat + PROVINCE + st.strat:PROVINCE,
                     random = ~st.strat|sppsite, # Allow random slopes by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                    method = "ML",
                    na.action = na.omit,
                    data = df.pb)
#Run graphical diagnostic plots
performance::check_model(pb.strat.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#tab_model(pb.strat.stbs)
res.pb.strat <- tidy(emtrends(pb.strat.stbs, pairwise ~ PROVINCE, var = "st.strat")$emtrends, conf.int = TRUE)
res.pb.strat$season <- rep("pb", nrow(res.pb.strat))
res.pb.strat$driver <- rep("strat", nrow(res.pb.strat))
colnames(res.pb.strat)[2] <- "trend"

#Breeding Season Temperature Model
b.temp.stbs <- lme(stbs ~ st.temp + PROVINCE + st.temp:PROVINCE,
                   random = ~st.temp|sppsite, # Allow random slopes by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                    method = "ML",
                    na.action = na.omit,
                    data = df.b)
#Run graphical diagnostic plots
performance::check_model(b.temp.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#tab_model(b.temp.stbs)
                random = ~st.temp|sppsite, # Allow random slopes by time series
res.b.temp$season <- rep("b", nrow(res.b.temp))
res.b.temp$driver <- rep("temp", nrow(res.b.temp))
colnames(res.b.temp)[2] <- "trend"

#Breeding Season Stratification Model
b.strat.stbs <- lme(stbs ~ st.strat + PROVINCE + st.strat:PROVINCE,
                    random = ~st.strat|sppsite, # Allow random slopes by time series
                    control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     na.action = na.omit,
                     data = df.b)
#Run graphical diagnostic plots
performance::check_model(b.strat.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#tab_model(pb.strat.stbs)
res.b.strat <- tidy(emtrends(b.strat.stbs, pairwise ~ PROVINCE, var = "st.strat")$emtrends, conf.int = TRUE)
res.b.strat$season <- rep("b", nrow(res.b.strat))
res.b.strat$driver <- rep("strat", nrow(res.b.strat))
colnames(res.b.strat)[2] <- "trend"

res.time <- tidy(emtrends(n.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) 
res.time$season <- rep("t", nrow(res.time))
res.time$driver <- rep("time", nrow(res.time))
colnames(res.time)[2] <- "trend"

res <- rbind(res.pb.temp, res.b.temp, res.pb.strat, res.b.strat, res.time)
res <- left_join(res, diversity_df, by = c("PROVINCE" = "province"))

#Assemble sppsite level trends
ranef.res.pb.temp <- ranef(pb.temp.stbs) %>% mutate(sppsite = rownames(ranef(pb.temp.stbs))) %>% remove_rownames()
ranef.res.pb.temp$season <- rep("pb", nrow(ranef.res.pb.temp))
ranef.res.pb.temp$driver <- rep("temp", nrow(ranef.res.pb.temp))
colnames(ranef.res.pb.temp)[2] <- "trend"
ranef.res.pb.temp <- left_join(ranef.res.pb.temp, unique(dplyr::select(dat, sppsite, ECOREGION, PROVINCE)))

ranef.res.b.temp <- ranef(b.temp.stbs) %>% mutate(sppsite = rownames(ranef(b.temp.stbs))) %>% remove_rownames()
ranef.res.b.temp$season <- rep("b", nrow(ranef.res.b.temp))
ranef.res.b.temp$driver <- rep("temp", nrow(ranef.res.b.temp))
colnames(ranef.res.b.temp)[2] <- "trend"
ranef.res.b.temp <- left_join(ranef.res.b.temp, unique(dplyr::select(dat, sppsite, ECOREGION, PROVINCE)))

ranef.res.pb.strat <- ranef(pb.strat.stbs) %>% mutate(sppsite = rownames(ranef(pb.strat.stbs))) %>% remove_rownames()
ranef.res.pb.strat$season <- rep("pb", nrow(ranef.res.pb.strat))
ranef.res.pb.strat$driver <- rep("strat", nrow(ranef.res.pb.strat))
colnames(ranef.res.pb.strat)[2] <- "trend"
ranef.res.pb.strat <- left_join(ranef.res.pb.strat, unique(dplyr::select(dat, sppsite, ECOREGION, PROVINCE)))

ranef.res.b.strat <- ranef(b.strat.stbs) %>% mutate(sppsite = rownames(ranef(b.strat.stbs))) %>% remove_rownames()
ranef.res.b.strat$season <- rep("b", nrow(ranef.res.b.strat))
ranef.res.b.strat$driver <- rep("strat", nrow(ranef.res.b.strat))
colnames(ranef.res.b.strat)[2] <- "trend"
ranef.res.b.strat <- left_join(ranef.res.b.strat, unique(dplyr::select(dat, sppsite, ECOREGION, PROVINCE)))

ranef.res.time <- ranef(n.trends) %>% mutate(sppsite = rownames(ranef(n.trends))) %>% remove_rownames()
ranef.res.time$season <- rep("t", nrow(ranef.res.time))
ranef.res.time$driver <- rep("time", nrow(ranef.res.time))
colnames(ranef.res.time)[2] <- "trend"
ranef.res.time <- left_join(ranef.res.time, unique(dplyr::select(dat, sppsite, ECOREGION, PROVINCE)))

ranef.res <- rbind(ranef.res.pb.temp, ranef.res.b.temp, ranef.res.pb.strat, ranef.res.b.strat, ranef.res.time)
ranef.res <- left_join(ranef.res, diversity_df, by = c("PROVINCE" = "province"))
#====
ggplot(filter(df.pb, PROVINCE == "Cold Temperate Northwest Atlantic")) + 
  geom_point(aes(st.temp, stbs, color = ECOREGION)) + 
  geom_smooth(aes(st.temp, stbs, color = ECOREGION), method = "loess", se = TRUE)

ggplot(filter(df.pb, PROVINCE == "Cold Temperate Northwest Atlantic")) + 
  geom_point(aes(yearno.x, st.temp, color = ECOREGION))
ggplot(filter(df.pb, PROVINCE == "Cold Temperate Northwest Atlantic")) + 
  geom_point(aes(yearno.x, stbs, color = ECOREGION))

ggplot(df.b) + 
  geom_point(aes(st.temp, stbs, color = as.factor(PROVINCE))) + 
  geom_smooth(aes(st.temp, stbs, color = as.factor(PROVINCE)))

ggplot(df.b) + 
  geom_point(aes(st.temp, stbs, color = as.factor(PROVINCE)))

res$trend <- as.numeric(res$trend)
res$Shannon_Index <- as.numeric((res$Shannon_Index))
res$PROVINCE <- as.factor(res$PROVINCE)
res$driver <- factor(res$driver, levels = c("time", "temp", "strat"))

cols <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#fcdc54")
driver.labs <- c("Time Series", "Temperature", "Stratification")
names(driver.labs) <- c("time", "temp", "strat")

plot <- ggplot() + 
  #geom_point(data = ranef.res, aes(Shannon_Index, trend, color = PROVINCE, shape = season), size = 4, alpha = 0.4) +
  geom_hline(data = res[complete.cases(res),], aes(yintercept = 0), linetype = "dotted", color = "black") + 
  geom_point(data = res[complete.cases(res),], aes(Shannon_Index, trend, color = PROVINCE, shape = season), size = 6) + 
  geom_smooth(data = res[complete.cases(res),], aes(Shannon_Index, trend), method = "lm", color = "black") +
  facet_wrap(~driver, labeller = labeller(driver = driver.labs)) + 
  labs(x = "Shannon-Wiener Index (H')", y = "Trend Estimate", color = "Ecosystem") + 
  scale_shape_discrete(name = "Model", labels = c("Breeding Season", "Pre-Breeding Season", "Time Series")) + 
  scale_color_manual(values = cols) + 
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
  
tab_model(lm(trend~Shannon_Index, data = filter(res, driver == "time")), show.ci = .9)
tab_model(lm(trend~Shannon_Index, data = filter(res, driver == "temp")), show.ci = .9)
tab_model(lm(trend~Shannon_Index, data = filter(res, driver == "strat")), show.ci = .9)

###GAMS
ggplot(filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific")) + 
  geom_point(aes(d.stpb_temp, d.stbs, color = sppsite)) + 
  geom_smooth(aes(d.stpb_temp, d.stbs, color = sppsite), method = "lm", formula = y ~ poly(x,2), se = FALSE) + 
  facet_wrap(~ECOREGION) + 
  guides(color = "none") + 
  theme_bw() + 
  labs(x = "Detrended, standardized pre-breeding temperature", y = "Detrended, standardized seabird breeding success")
