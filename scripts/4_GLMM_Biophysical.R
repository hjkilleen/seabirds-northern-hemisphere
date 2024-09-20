#Select detrended generalized linear mixed effects models of seabird breeding success responses to ecoregional temperature and stratification conditions. Models are constructed for each ecosystem to account for ecosystem-specific covariance relationships between temperature and stratification. 

#Written by Helen Killeen

# Thu Sep 19 16:15:20 2024 ------------------------------

#In this analysis, we use the nlme package to fit linear mixed models because random effect variance is too low to permit optimization using the lme4 approach. Random terms (to account for differences among time series) do not influence model results using intercept-only, slope-only or a combined structure. They are included in the models below because they are required by our experimental design. 

#For each ecosystem, we evaluate predictor covariance, fit models for pre-breeding season and breeding season temperature and stratification separated, and print out HTML versions of model summary tables for use in the supplementary materials. Collinearity values, model estimates, and 95% confidence intervals are stored manually in biophysical_glmm_summary.xlsx.

#SOURCES
#====
source("scripts/0_Seabird_Helpers.R")

combined.df <- readRDS("data/combined.df.rds")
#=====

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

tab_model(arc.pb.temp)#Table S15.1
saveRDS(arc.pb.temp, file = "output/biophysical_models/arc.pb.temp.rds")#save model output

#stratification
arc.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Arctic"))

tab_model(arc.pb.strat)#Table S15.2
saveRDS(arc.pb.strat, file = "output/biophysical_models/arc.pb.strat.rds")#save model output

#fit breeding season models for Arctic time series
#temperature
arc.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Arctic"))

tab_model(arc.b.temp)#Table S16.1
saveRDS(arc.b.temp, file = "output/biophysical_models/arc.b.temp.rds")#save model output

#stratification
arc.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Arctic"))

tab_model(arc.b.strat)#Table S16.2
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

tab_model(nes.pb.temp)#Table S17.1
saveRDS(nes.pb.temp, file = "output/biophysical_models/nes.pb.temp.rds")#save model output

#stratification
nes.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Northern European Seas"))

tab_model(nes.pb.strat)#Table S17.2
saveRDS(nes.pb.strat, file = "output/biophysical_models/nes.pb.strat.rds")#save model output

#fit breeding season models for Northern European Seas time series
#temperature
nes.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Northern European Seas"))

tab_model(nes.b.temp)#Table S18.1
saveRDS(nes.b.temp, file = "output/biophysical_models/nes.b.temp.rds")#save model output

#stratification
nes.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Northern European Seas"))

tab_model(nes.b.strat)#Table S18.2
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

tab_model(nwa.pb.temp)#Table S19.1
saveRDS(nwa.pb.temp, file = "output/biophysical_models/nwa.pb.temp.rds")#save model output

#stratification
nwa.pb.strat <- lme(d.stbs ~  d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))

tab_model(nwa.pb.strat)#Table S19.2
saveRDS(nwa.pb.strat, file = "output/biophysical_models/nwa.pb.strat.rds")#save model output

#fit breeding season models for Cold Temperate Northwest Atlantic time series
#temperature
nwa.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))

tab_model(nwa.b.temp)#Table S20.1
saveRDS(nwa.b.temp, file = "output/biophysical_models/nwa.b.temp.rds")#save model output

#stratification
nwa.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))

tab_model(nwa.b.strat)#Table S20.2
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

tab_model(nep.pb.temp)#Table S21.1
saveRDS(nep.pb.temp, file = "output/biophysical_models/nep.pb.temp.rds")#save model output

#stratification
nep.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))

tab_model(nep.pb.strat)#Table S21.2
saveRDS(nep.pb.strat, file = "output/biophysical_models/nep.pb.strat.rds")#save model output

#fit breeding season models for Cold Temperate Northeast Pacific time series
#temperature
nep.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))

tab_model(nep.b.temp)#Table S22.1
saveRDS(nep.b.temp, file = "output/biophysical_models/nep.b.temp.rds")#save model output

#stratification
nep.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))

tab_model(nep.b.strat)#Table S22.2
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

tab_model(wnep.pb.temp)#Table S23.1
saveRDS(wnep.pb.temp, file = "output/biophysical_models/wnep.pb.temp.rds")#save model output

#stratification
wnep.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                    random = ~ 1|sppsite, # Allow random intercepts by time series
                    control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                    method = "REML",
                    data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

tab_model(wnep.pb.strat)#Table S23.2
saveRDS(wnep.pb.strat, file = "output/biophysical_models/wnep.pb.strat.rds")#save model output

#fit breeding season models for Warm Temperate Northeast Pacific time series
#temperature
wnep.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

tab_model(wnep.b.temp)#Table S24.1
saveRDS(wnep.b.temp, file = "output/biophysical_models/wnep.b.temp.rds")

#stratification
wnep.b.strat <- lme(d.stbs ~ d.stb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

tab_model(wnep.b.strat)#Table S24.2
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

tab_model(nwp.pb.temp)#Table S25.1
saveRDS(nwp.pb.temp, file = "output/biophysical_models/nwp.pb.temp.rds")#save model output

#stratification
nwp.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))

tab_model(nwp.pb.strat)#Table S25.2
saveRDS(nwp.pb.strat, file = "output/biophysical_models/nwp.pb.strat.rds")#save model output

#fit breeding season models for Cold Temperate Northwest Pacific time series
#temperature
nwp.b.temp <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))

tab_model(nwp.b.temp)#Table S26.1
saveRDS(nwp.b.temp, file = "output/biophysical_models/nwp.b.temp.rds")#save model output

#stratification
nwp.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))

tab_model(nwp.b.strat)#Table S26.2
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

tab_model(haw.pb.temp)#Table S27.1
saveRDS(haw.pb.temp, file = "output/biophysical_models/haw.pb.temp.rds")#save model output

#stratification
haw.pb.strat <- lme(d.stbs ~ d.stpb_strat,
                   random = ~ 1|sppsite, # Allow random intercepts by time series
                   control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                   method = "REML",
                   data = filter(combined.df, PROVINCE == "Hawaii"))

tab_model(haw.pb.strat)#Table S27.2
saveRDS(haw.pb.strat, file = "output/biophysical_models/haw.pb.strat.rds")#save model output

#fit breeding season models for Hawaii time series
#temperature
haw.b.temp <- lme(d.stbs ~ d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Hawaii"))

tab_model(haw.b.temp)#Table S28.1
saveRDS(haw.b.temp, file = "output/biophysical models by province/haw.b.temp.rds")#save model output

#stratification
haw.b.strat <- lme(d.stbs ~ d.stb_strat,
                  random = ~ 1|sppsite, # Allow random intercepts by time series
                  control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
                  method = "REML",
                  data = filter(combined.df, PROVINCE == "Hawaii"))

tab_model(haw.b.strat)#Table S28.2
saveRDS(haw.b.strat, file = "output/biophysical models by province/haw.b.strat.rds")#save model output
#====

#Go to table_biophysicalOutput for summary table included in manuscript