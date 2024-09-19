#Select detrended generalized linear mixed effects models of seabird breeding success responses to ecoregional temperature and stratification conditions. Models are constructured for each ecosystem to account for ecosystem-specific covariance relationships between temperature and stratification. 

#Written by Helen Killeen

# Thu Sep 19 13:55:30 2024 ------------------------------

#In this analysis, we use the nlme package to fit linear mixed models because random effect variance is too low to permit optimization using the lme4 approach. Random terms (to account for differences among time series) do not influence model results using intercept-only, slope-only or a combined structure. They are included in the models below because they are required by our experimental design. 

#For each ecosystem, we evaluate predictor covariance, fit the most appropriate model for pre-breeding season and breeding season conditions based on the variance inflation factor and covariance structure, and print out HTML versions of model summary tables for use in the supplementary materials.

#SOURCES
#====
source("scripts/0_Seabird_Helpers.R")

combined.df <- readRDS("data/combined.df.rds")
#=====

#Arctic Models
#====
#Investigate covariance among predictors. Weak collinearity within seasons, include all terms in models.
cor(dplyr::select(filter(combined.df, PROVINCE == "Arctic"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit full pre-breeding season model for Arctic time series
arc.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Arctic"))

tab_model(arc.pb.fin)#Table S15
saveRDS(arc.pb.fin, file = "output/biophysical_models/arc.pb.fin.rds")#save model output

#fit full breeding season model for Arctic time series
arc.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Arctic"))

tab_model(arc.b.fin)#Table S16
saveRDS(arc.b.fin, file = "output/biophysical_models/arc.b.fin.rds")#save model output
#====

#Northern European Seas Models
#====
#Investigate correlation among predictors. Breeding season covariates are collinear (~0.67), so check variance inflation factor for breeding season terms. 
cor(dplyr::select(filter(combined.df, PROVINCE == "Northern European Seas"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#calculate variance inflation factor for breeding season model, looks okay (VIF<4) to include all terms. 
vif(lm(d.stbs ~ d.stb_strat + d.stb_temp,
       data = filter(combined.df, PROVINCE == "Northern European Seas")))

#fit full pre-breeding season model for Northern European Seas time series
nes.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Northern European Seas"))

tab_model(nes.pb.fin)#Table S17
saveRDS(nes.pb.fin, file = "output/biophysical_models by province/nes.pb.fin.rds")#save model output

#fit full breeding season model for Northern European Seas time series
nes.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Northern European Seas"))

tab_model(nes.b.fin)#Table S18
saveRDS(nes.b.fin, file = "output/biophysical_models/nes.b.fin.rds")#save model output
#====

#Cold Temperate Northwest Atlantic Models
#====
#Investigate correlation among predictors. Correlated terms are across seasons, looks okay to proceed.
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit full pre-breeding season model for Cold Temperate Northwest Atlantic time series
nwa.pb.fin <- lme(d.stbs ~  d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))

tab_model(nwa.pb.fin)#Table S19
saveRDS(nwa.pb.fin, file = "output/biophysical_models/nwa.pb.fin.rds")#save model output

#fit full breeding season model for Cold Temperate Northwest Atlantic time series
nwa.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))

tab_model(nwa.b.fin)#Table S20
saveRDS(nwa.b.fin, file = "output/biophysical_models/nwa.b.fin.rds")#save model output
#====

#Cold Temperate Northeast Pacific Models
#====
#Investigate correlation among predictors. All pre-breeding season stratification and temperature are collinear (.70-.75), so check variance inflation factor for both seasonal models. 
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#calculate variance inflation factor for both seasonal models. 
vif(lm(d.stbs ~ d.stpb_strat + d.stpb_temp,#pre-breeding season terms look okay
       data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific")))

vif(lm(d.stbs ~ d.stb_strat + d.stb_temp,#breeding season terms okay as well
       data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific")))

#fit full pre-breeding season model for Cold Temperate Northeast Pacific time series
nep.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))

tab_model(nep.pb.fin)#Table S21
saveRDS(nep.pb.fin, file = "output/biophysical_models/nep.pb.fin.rds")#save model output

#fit full breeding season model for Cold Temperate Northeast Pacific time series
nep.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))

tab_model(nep.b.fin)#Table S22
saveRDS(nep.b.fin, file = "output/biophysical_models/nep.b.fin.rds")#save model output
#====

#Warm Temperate Northeast Pacific Models
#====
#Investigate correlation among predictors. Temperature and stratification across both seasons are correlated (.60-.80), so check variance inflation factor for both seasonal models. 
cor(dplyr::select(filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#calculate variance inflation factor for breeding season model. Collinearity between temperature and stratification is more pronounced in the breeding season, but it looks okay to include all terms (VIF<4).
vif(lm(d.stbs ~ d.stpb_strat + d.stpb_temp,
       data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific")))

vif(lm(d.stbs ~ d.stb_strat + d.stb_temp,
       data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific")))

#fit full pre-breeding season model for Warm Temperate Northeast Pacific time series
wnep.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

tab_model(wnep.pb.fin)#Table S23
saveRDS(wnep.pb.fin, file = "output/biophysical_models/wnep.pb.fin.rds")#save model output

#fit full breeding season model for Warm Temperate Northeast Pacific time series
wnep.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

tab_model(wnep.b.fin)
saveRDS(wnep.b.fin, file = "output/biophysical_models/wnep.b.fin.rds")
#====

#Cold Temperate Northwest Pacific Models
#====
#Investigate correlation among predictors. Corrleations are across seasons, so looks okay to proceed. 
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit full pre-breeding season model for Cold Temperate Northwest Pacific time series
nwp.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))

tab_model(nwp.pb.fin)#Table S25
saveRDS(nwp.pb.fin, file = "output/biophysical_models/nwp.pb.fin.rds")#save model output

#fit full breeding season model for Cold Temperate Northwest Pacific time series
nwp.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))

tab_model(nwp.b.fin)#Table S26
saveRDS(nwp.b.fin, file = "output/biophysical_models/nwp.b.fin.rds")#save model output
#====

#Hawaii Models
#====
#Investigate correlation among predictors. Temperature and stratification are correlated across both seasons (.70-.85), so check variance inflation factor both seasonal models.
cor(dplyr::select(filter(combined.df, PROVINCE == "Hawaii"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#calculate variance inflation factor for both seasonal models. Correlation is more pronounced between temperature and stratification during the pre-breeding season, but it looks okay to include all terms (VIF<4). 
vif(lm(d.stbs ~ d.stpb_strat + d.stpb_temp,
       data = filter(combined.df, PROVINCE == "Hawaii")))

vif(lm(d.stbs ~ d.stb_strat + d.stb_temp,
       data = filter(combined.df, PROVINCE == "Hawaii")))

#fit full pre-breeding season model for Hawaii time series
haw.pb.fin <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Hawaii"))

tab_model(haw.pb.fin)#Table S27
saveRDS(haw.pb.fin, file = "output/biophysical_models/haw.pb.fin.rds")#save model output

#fit full breeding season model for Hawaii time series
haw.b.fin <- lme(d.stbs ~ d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Hawaii"))

tab_model(haw.b.fin)#Table S28
saveRDS(haw.b.fin, file = "output/biophysical models by province/haw.b.fin.rds")#save model output
#====