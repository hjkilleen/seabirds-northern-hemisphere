#Select detrended generalized linear mixed effects models of seabird breeding success responses to ecoregional temperature and stratification conditions. Models are constructed for each ecosystem to account for ecosystem-specific covariance relationships between temperature and stratification. 

#Written by Helen Killeen

# Wed Apr  2 10:25:43 2025 ------------------------------

#In this analysis, we use the nlme package to fit linear mixed models because random effect variance is too low to permit optimization using the lme4 approach. Random terms (to account for differences among time series) do not influence model results using intercept-only, slope-only or a combined structure. They are included in the models below because they are required by our experimental design. 

#For each ecosystem, we evaluate predictor covariance, fit models for pre-breeding season and breeding season temperature and stratification separated, and print out HTML versions of model summary tables for use in the supplementary materials. Collinearity values, model estimates, and 95% confidence intervals are stored manually in biophysical_glmm_summary.xlsx.

#SOURCES
#====
source("scripts/0_Seabird_Helpers.R")

combined.df <- readRDS("data/combined.df.rds")
df.pb <- readRDS("data/df.pb.rds")
df.b <- readRDS("data/df.b.rds")
dat <- readRDS("data/dat.Rda")
n.trends <- readRDS("output/n.trends.rds")
#=====

#BIOPHYSICAL MODELS (FAMILY THREE IN SUPPLEMENTARY MATERIALS)
#====
#Set-up
df.pb <- left_join(df.pb, dat, by = c("year", "ECOREGION", "PROVINCE"))
df.b <- left_join(df.b, dat, by = c("year", "ECOREGION", "PROVINCE"))

#Pre-Breeding Season Temperature Model
pb.temp.stbs <- lme(stbs ~ st.temp + PROVINCE + st.temp:PROVINCE,
                    random = ~st.temp|sppsite, # Allow random slopes and intercepts by time series
                    control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                    method = "ML",
                    na.action = na.omit,
                    data = df.pb)
#Run graphical diagnostic plots
performance::check_model(pb.temp.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#Model results
tab_model(pb.temp.stbs, show.ci = .9)#S15.1
tidy(confint(emtrends(pb.temp.stbs, pairwise ~ PROVINCE, var = "st.temp"), level = 0.9)$emtrends, conf.int  = TRUE) %>%
  kbl(caption = "Using 90% confidence interval", digits = 3) %>% 
  kable_classic()#S15.2
#extract model results
res.pb.temp <- tidy(emtrends(pb.temp.stbs, pairwise ~ PROVINCE, var = "st.temp")$emtrends, conf.int = TRUE)
res.pb.temp$season <- rep("pb", nrow(res.pb.temp))
res.pb.temp$driver <- rep("temp", nrow(res.pb.temp))
colnames(res.pb.temp)[2] <- "trend"

#Breeding Season Temperature Model
b.temp.stbs <- lme(stbs ~ st.temp + PROVINCE + st.temp:PROVINCE,
                   random = ~st.temp|sppsite, # Allow random slopes by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "ML",
                   na.action = na.omit,
                   data = df.b)
#Run graphical diagnostic plots
performance::check_model(b.temp.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#Model results
tab_model(b.temp.stbs, show.ci = .9)#S15.3
tidy(confint(emtrends(b.temp.stbs, pairwise ~ PROVINCE, var = "st.temp"), level = 0.9)$emtrends, conf.int  = TRUE) %>%
  kbl(caption = "Using 90% confidence interval", digits = 3) %>% 
  kable_classic()#S15.4
#extract model results
res.b.temp <- tidy(emtrends(b.temp.stbs, pairwise ~ PROVINCE, var = "st.temp")$emtrends, conf.int = TRUE)
res.b.temp$season <- rep("b", nrow(res.b.temp))
res.b.temp$driver <- rep("temp", nrow(res.b.temp))
colnames(res.b.temp)[2] <- "trend"

#Pre-Breeding Season Stratification Model
pb.strat.stbs <- lme(stbs ~ st.strat + PROVINCE + st.strat:PROVINCE,
                     random = ~st.strat|sppsite, # Allow random slopes and intercepts by time series
                     control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                     method = "ML",
                     na.action = na.omit,
                     data = df.pb)
#Run graphical diagnostic plots
performance::check_model(pb.strat.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#Model results
tab_model(pb.strat.stbs, show.ci = 0.9)#S16.1
tidy(confint(emtrends(pb.strat.stbs, pairwise ~ PROVINCE, var = "st.strat"), level = 0.9)$emtrends, conf.int  = TRUE) %>%
  kbl(caption = "Using 90% confidence interval", digits = 3) %>% 
  kable_classic()#S16.2
#Extract model results
res.pb.strat <- tidy(emtrends(pb.strat.stbs, pairwise ~ PROVINCE, var = "st.strat")$emtrends, conf.int = TRUE)
res.pb.strat$season <- rep("pb", nrow(res.pb.strat))
res.pb.strat$driver <- rep("strat", nrow(res.pb.strat))
colnames(res.pb.strat)[2] <- "trend"

#Breeding Season Stratification Model
b.strat.stbs <- lme(stbs ~ st.strat + PROVINCE + st.strat:PROVINCE,
                    random = ~st.strat|sppsite, # Allow random slopes by time series
                    control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                    method = "ML",
                    na.action = na.omit,
                    data = df.b)
#Run graphical diagnostic plots
performance::check_model(b.strat.stbs, check = c("qq", "linearity", "normality"))#looks good for all tests
#Model results
tab_model(b.strat.stbs, show.ci = 0.9)#S16.3
tidy(confint(emtrends(b.strat.stbs, pairwise ~ PROVINCE, var = "st.strat"), level = 0.9)$emtrends, conf.int  = TRUE) %>%
  kbl(caption = "Using 90% confidence interval", digits = 3) %>% 
  kable_classic()#S16.4
#Extract model results
res.b.strat <- tidy(emtrends(b.strat.stbs, pairwise ~ PROVINCE, var = "st.strat")$emtrends, conf.int = TRUE)
res.b.strat$season <- rep("b", nrow(res.b.strat))
res.b.strat$driver <- rep("strat", nrow(res.b.strat))
colnames(res.b.strat)[2] <- "trend"
#====

#CREATE COMBINED MODEL RESULTS DATAFRAME FOR PLOTTING
#====
#Extract model results from the seabird breeding success time series model
res.time <- tidy(emtrends(n.trends, pairwise ~ PROVINCE, var = "yearno")$emtrends, conf.int = TRUE) 
res.time$season <- rep("t", nrow(res.time))
res.time$driver <- rep("time", nrow(res.time))
colnames(res.time)[2] <- "trend"

#Combine all model results tables
res <- rbind(res.pb.temp, res.b.temp, res.pb.strat, res.b.strat, res.time)
res <- left_join(res, diversity_df, by = c("PROVINCE" = "province"))#Add in Shannon-Wiener diversity indices for plotting
saveRDS(res, "output/trend_diversity_estimates.rds")#save results file
#====

#Corresponding figure produced in figure_portfolio_effect




#DETRENDED BIOPHYSICAL MODELS (FAMILY FOUR IN SUPPLEMENTARY MATERIALS)
#====
#Arctic Models
#====
#Investigate covariance among predictors. Weak collinearity within seasons.
cor(dplyr::select(filter(combined.df, PROVINCE == "Arctic"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Arctic time series
#temperature
arc.pb.temp <- lme(d.stbs ~ d.stpb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Arctic"))
check_model(arc.pb.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(arc.pb.temp, show.ci = 0.9)#Table S17.1
saveRDS(arc.pb.temp, file = "output/biophysical_models/arc.pb.temp.rds")#save model output

#stratification
arc.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Arctic"))
check_model(arc.pb.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(arc.pb.strat, show.ci = 0.9)#Table S17.2
saveRDS(arc.pb.strat, file = "output/biophysical_models/arc.pb.strat.rds")#save model output

#fit breeding season models for Arctic time series
#temperature
arc.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Arctic"))
check_model(arc.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(arc.b.temp, show.ci = 0.9)#Table S18.1
saveRDS(arc.b.temp, file = "output/biophysical_models/arc.b.temp.rds")#save model output

#stratification
arc.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Arctic"))
check_model(arc.b.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(arc.b.strat, show.ci = 0.9)#Table S18.2
saveRDS(arc.b.strat, file = "output/biophysical_models/arc.b.strat.rds")#save model output
#====

#Northern European Seas Models
#====
#Investigate correlation among predictors. Breeding season covariates are moderately collinear (~0.67).
cor(dplyr::select(filter(combined.df, PROVINCE == "Northern European Seas"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Northern European Seas time series
#temperature
nes.pb.temp <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Northern European Seas"))
check_model(nes.pb.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nes.pb.temp, show.ci = 0.9)#Table S19.1
saveRDS(nes.pb.temp, file = "output/biophysical_models/nes.pb.temp.rds")#save model output

#stratification
nes.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Northern European Seas"))
check_model(nes.pb.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nes.pb.strat, show.ci = 0.9)#Table S19.2
saveRDS(nes.pb.strat, file = "output/biophysical_models/nes.pb.strat.rds")#save model output

#fit breeding season models for Northern European Seas time series
#temperature
nes.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Northern European Seas"))
check_model(nes.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nes.b.temp, show.ci = 0.9)#Table S20.1
saveRDS(nes.b.temp, file = "output/biophysical_models/nes.b.temp.rds")#save model output

#stratification
nes.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Northern European Seas"))
check_model(nes.b.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nes.b.strat, show.ci = 0.9)#Table S20.2
saveRDS(nes.b.strat, file = "output/biophysical_models/nes.b.strat.rds")#save model output
#====

#Cold Temperate Northwest Atlantic Models
#====
#Investigate correlation among predictors. Correlated terms are across seasons.
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Cold Temperate Northwest Atlantic time series
#temperature
nwa.pb.temp <- lme(d.stbs ~  d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))
check_model(nwa.pb.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwa.pb.temp, show.ci = 0.9)#Table S21.1
saveRDS(nwa.pb.temp, file = "output/biophysical_models/nwa.pb.temp.rds")#save model output

#stratification
nwa.pb.strat <- lme(d.stbs ~  d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))
check_model(nwa.pb.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwa.pb.strat, show.ci = 0.9)#Table S21.2
saveRDS(nwa.pb.strat, file = "output/biophysical_models/nwa.pb.strat.rds")#save model output

#fit breeding season models for Cold Temperate Northwest Atlantic time series
#temperature
nwa.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))
check_model(nwa.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwa.b.temp, show.ci = 0.9)#Table S22.1
saveRDS(nwa.b.temp, file = "output/biophysical_models/nwa.b.temp.rds")#save model output

#stratification
nwa.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))
check_model(pb.chl.trends, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwa.b.strat, show.ci = 0.9)#Table S22.2
saveRDS(nwa.b.strat, file = "output/biophysical_models/nwa.b.strat.rds")#save model output
#====

#Cold Temperate Northeast Pacific Models
#====
#Investigate correlation among predictors.Pre-breeding season stratification and temperature are collinear (.70-.75).
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Cold Temperate Northeast Pacific time series
#temperature
nep.pb.temp <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))
check_model(nep.pb.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nep.pb.temp, show.ci = 0.9)#Table S23.1
saveRDS(nep.pb.temp, file = "output/biophysical_models/nep.pb.temp.rds")#save model output

#stratification
nep.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))
check_model(nep.pb.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nep.pb.strat, show.ci = 0.9)#Table S23.2
saveRDS(nep.pb.strat, file = "output/biophysical_models/nep.pb.strat.rds")#save model output

#fit breeding season models for Cold Temperate Northeast Pacific time series
#temperature
nep.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))
check_model(nep.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nep.b.temp, show.ci = 0.9)#Table S24.1
saveRDS(nep.b.temp, file = "output/biophysical_models/nep.b.temp.rds")#save model output

#stratification
nep.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))
check_model(nep.b.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nep.b.strat, show.ci = 0.9)#Table S24.2
saveRDS(nep.b.strat, file = "output/biophysical_models/nep.b.strat.rds")#save model output
#====

#Warm Temperate Northeast Pacific Models
#====
#Investigate correlation among predictors. Temperature and stratification across both seasons are correlated (.75-.80).
cor(dplyr::select(filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Warm Temperate Northeast Pacific time series
#temperature
wnep.pb.temp <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))
check_model(wnep.pb.temp, check = c("qq", "linearity", "normality"))#low fitted values have larger (positive) residuals

tab_model(wnep.pb.temp, show.ci = 0.9)#Table S25.1
saveRDS(wnep.pb.temp, file = "output/biophysical_models/wnep.pb.temp.rds")#save model output

#stratification
wnep.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                    random = ~ 1|sppsite, # Allow random intercepts by time series
                    control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                    method = "REML",
                    data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))
check_model(wnep.pb.strat, check = c("qq", "linearity", "normality"))#looks okay for all tests

tab_model(wnep.pb.strat, show.ci = 0.9)#Table S25.2
saveRDS(wnep.pb.strat, file = "output/biophysical_models/wnep.pb.strat.rds")#save model output

#fit breeding season models for Warm Temperate Northeast Pacific time series
#temperature
wnep.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))
check_model(wnep.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(wnep.b.temp, show.ci = 0.9)#Table S26.1
saveRDS(wnep.b.temp, file = "output/biophysical_models/wnep.b.temp.rds")

#stratification
wnep.b.strat <- lme(d.stbs ~ d.stb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))
check_model(wnep.b.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(wnep.b.strat, show.ci = 0.9)#Table S26.2
saveRDS(wnep.b.strat, file = "output/biophysical_models/wnep.b.strat.rds")
#====

#Cold Temperate Northwest Pacific Models
#====
#Investigate correlation among predictors. Corrleations are across seasons.
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Cold Temperate Northwest Pacific time series
#temperature
nwp.pb.temp <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))
check_model(nwp.pb.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwp.pb.temp, show.ci = 0.9)#Table S27.1
saveRDS(nwp.pb.temp, file = "output/biophysical_models/nwp.pb.temp.rds")#save model output

#stratification
nwp.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))
check_model(nwp.pb.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwp.pb.strat, show.ci = 0.9)#Table S27.2
saveRDS(nwp.pb.strat, file = "output/biophysical_models/nwp.pb.strat.rds")#save model output

#fit breeding season models for Cold Temperate Northwest Pacific time series
#temperature
nwp.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))
check_model(nwp.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwp.b.temp, show.ci = 0.9)#Table S28.1
saveRDS(nwp.b.temp, file = "output/biophysical_models/nwp.b.temp.rds")#save model output

#stratification
nwp.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))
check_model(nwp.b.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(nwp.b.strat, show.ci = 0.9)#Table S28.2
saveRDS(nwp.b.strat, file = "output/biophysical_models/nwp.b.strat.rds")#save model output
#====

#Hawaii Models
#====
#Investigate correlation among predictors. Temperature and stratification are correlated across both seasons (.70-.85).
cor(dplyr::select(filter(combined.df, PROVINCE == "Hawaii"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit pre-breeding season models for Hawaii time series
#temperature
haw.pb.temp <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Hawaii"))
check_model(haw.pb.temp, check = c("qq", "linearity", "normality"))#tails of the distribution have larger (negative residuals)

tab_model(haw.pb.temp, show.ci = 0.9)#Table S29.1
saveRDS(haw.pb.temp, file = "output/biophysical_models/haw.pb.temp.rds")#save model output

#stratification
haw.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Hawaii"))
check_model(haw.pb.strat, check = c("qq", "linearity", "normality"))#left-hand side of the distribution has larger (negative residuals)

tab_model(haw.pb.strat, show.ci = 0.9)#Table S29.2
saveRDS(haw.pb.strat, file = "output/biophysical_models/haw.pb.strat.rds")#save model output

#fit breeding season models for Hawaii time series
#temperature
haw.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Hawaii"))
check_model(haw.b.temp, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(haw.b.temp, show.ci = 0.9)#Table S30.1
saveRDS(haw.b.temp, file = "output/biophysical_models/haw.b.temp.rds")#save model output

#stratification
haw.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Hawaii"))
check_model(haw.b.strat, check = c("qq", "linearity", "normality"))#looks great for all tests

tab_model(haw.b.strat, show.ci = 0.9)#Table S30.2
saveRDS(haw.b.strat, file = "output/biophysical_models/haw.b.strat.rds")#save model output
#====
#====

#Go to table_biophysicalOutput for summary table included in manuscript

#Go to 5_Prey_Portfolio_Diversity