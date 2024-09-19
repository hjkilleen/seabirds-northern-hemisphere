#DETRENDED BREEDING SUCCESS RESPONSE TO ENVIRONMENT - MODEL SELECTION
#Helen Killeen

# Tue Apr 30 10:31:44 2024 ------------------------------

#In this analysis, we use the nlme package to fit linear mixed models because random effect variance is too low to permit optimization using the lme4 approach. Random terms (to account for differences among time series) do not influence model results using intercept-only, slope-only of a combined structure. They are included in the models below because they are required by our experimental design. 

#LIBRARIES & SOURCES
#====
source("scripts/0_Seabird_Helpers.R")
library(emmeans)
library(marmap)
library(rnaturalearth)
library(lme4)
library(sjPlot)
#=====

#LOAD DATA & SETUP
#====
#bind all breeding success/environment time series into a single data frame
files  <- list.files("data/sppsite_environment/", full.names = TRUE)
tables <- lapply(files, read_csv)
combined.df <- do.call(rbind , tables)

#drop the Med
combined.df <- combined.df %>% mutate_if(is.character, as.factor)#convert new variables to factors
combined.df <- droplevels(filter(combined.df, PROVINCE != "Mediterranean Sea"))

#save dataframe to output for plotting
saveRDS(combined.df, "output/combined.df.rds")
#====

#Arctic Models
#====
#Investigate covariance among predictors. Weak collinearity, include all terms in models.
cor(dplyr::select(filter(combined.df, PROVINCE == "Arctic"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#fit full pre-breeding season model for Arctic time series
arc.pb.fin <- lme(d.stbs ~ d.stpb_chl + d.stpb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Arctic"))

summary(arc.pb.fin)
tab_model(arc.pb.fin)
saveRDS(arc.pb.fin, file = "output/biophysical models by province/arc.pb.fin.rds")

#fit full breeding season model for Arctic time series
arc.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
           method = "REML",
           data = filter(combined.df, PROVINCE == "Arctic"))

Anova(arc.b.fin) # Remaining model terms, supplementary table
summary(arc.b.fin)
tab_model(arc.b.fin)
saveRDS(arc.b.fin, file = "output/biophysical models by province/arc.b.fin.rds")
#====

#Northern European Seas Models
#====
#Investigate correlation among predictors. Breeding season covariates are collinear (~0.67), so check variance inflation factor for breeding season terms. 
cor(dplyr::select(filter(combined.df, PROVINCE == "Northern European Seas"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#calculate variance inflation factor for breeding season model, looks okay to include all terms. 
vif(lm(d.stbs ~ d.stb_strat + d.stb_temp,
       data = filter(combined.df, PROVINCE == "Northern European Seas")))

#fit full pre-breeding season model for Northern European Seas time series
nes.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Northern European Seas"))

Anova(nes.pb.fin) # Remaining model term, supplementary table
summary(nes.pb.fin)
tab_model(nes.pb.fin)
saveRDS(nes.pb.fin, file = "output/biophysical models by province/nes.pb.fin.rds")

#fit full breeding season model for Northern European Seas time series
nes.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Northern European Seas"))

Anova(nes.b.fin) # Remaining model terms, supplementary table
summary(nes.b.fin)
tab_model(nes.b.fin)
saveRDS(nes.b.fin, file = "output/biophysical models by province/nes.b.fin.rds")
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

Anova(nwa.pb.fin) # Model term, supplementary table
summary(nwa.pb.fin)
tab_model(nwa.pb.fin)
saveRDS(nwa.pb.fin, file = "output/biophysical models by province/nwa.pb.fin.rds")

#fit full breeding season model for Cold Temperate Northwest Atlantic time series
nwa.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Atlantic"))

Anova(nwa.b.fin) # Remaining model terms, supplementary table
summary(nwa.b.fin)
tab_model(nwa.b.fin)
saveRDS(nwa.b.fin, file = "output/biophysical models by province/nwa.b.fin.rds")
#====

#Cold Temperate Northeast Pacific Models
#====
#Investigate correlation among predictors. All pre-breeding season stratification and temperature are collinear (.57-.80), so check variance inflation factor for both seasonal models. 
cor(dplyr::select(filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"), d.stbs, d.stpb_strat, d.stb_strat, d.stpb_temp, d.stb_temp))

#calculate variance inflation factor for both seasonal models. 
vif(lm(d.stbs ~ d.stpb_strat + d.stpb_temp,#pre-breeding season terms look okay
       data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific")))

vif(lm(d.stbs ~ d.stb_strat + d.stb_temp,#breeding season terms are more collinear, drop stratification
       data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific")))

#fit full pre-breeding season model for Cold Temperate Northeast Pacific time series
nep.pb.fin <- lme(d.stbs ~ d.stpb_temp + d.stpb_strat,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))


Anova(nep.pb.fin) # Remaining model term, supplementary table
summary(nep.pb.fin)
tab_model(nep.pb.fin)
saveRDS(nep.pb.fin, file = "output/biophysical models by province/nep.pb.fin.rds")

#fit full breeding season model for Cold Temperate Northeast Pacific time series
nep.b.fin <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northeast Pacific"))

Anova(nep.b.fin) # Remaining model terms, supplementary table
summary(nep.b.fin)
tab_model(nep.b.fin)
saveRDS(nep.b.fin, file = "output/biophysical models by province/nep.b.fin.rds")
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
wnep.pb.fin <- lme(d.stbs ~ d.stpb_temp,
              random = ~ 1|sppsite, # Allow random intercepts by time series
              control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
              method = "REML",
              data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

Anova(wnep.pb.fin) # Remaining model term, supplementary table
summary(wnep.pb.fin)
tab_model(wnep.pb.fin)
saveRDS(wnep.pb.fin, file = "output/biophysical models by province/wnep.pb.fin.rds")

#fit full breeding season model for Warm Temperate Northeast Pacific time series
wnep.b.fin <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Warm Temperate Northeast Pacific"))

Anova(wnep.b.fin) # Remaining model terms, supplementary table
summary(wnep.b.fin)
tab_model(wnep.b.fin)
saveRDS(wnep.b.fin, file = "output/biophysical models by province/wnep.b.fin.rds")
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

Anova(nwp.pb.fin) # Remaining model term, supplementary table
summary(nwp.pb.fin)
tab_model(nwp.pb.fin)
saveRDS(nwp.pb.fin, file = "output/biophysical models by province/nwp.pb.fin.rds")

#fit full breeding season model for Cold Temperate Northwest Pacific time series
nwp.b.fin <- lme(d.stbs ~ d.stb_temp + d.stb_strat,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Cold Temperate Northwest Pacific"))

Anova(nwp.b.fin) # Remaining model terms, supplementary table
summary(nwp.b.fin)
tab_model(nwp.b.fin)
saveRDS(nwp.b.fin, file = "output/biophysical models by province/nwp.b.fin.rds")
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

Anova(haw.pb.fin) # Remaining model term, supplementary table
summary(haw.pb.fin)
tab_model(haw.pb.fin)
saveRDS(haw.pb.fin, file = "output/biophysical models by province/haw.pb.fin.rds")

#fit full breeding season model for Hawaii time series
haw.b.fin <- lme(d.stbs ~ d.stb_temp,
             random = ~ 1|sppsite, # Allow random intercepts by time series
             control = list(maxIter = 10000, niterEM = 10000), # Give it time to converge
             method = "REML",
             data = filter(combined.df, PROVINCE == "Hawaii"))

Anova(haw.b.fin) # Remaining model terms, supplementary table
summary(haw.b.fin)
tab_model(haw.b.fin)
saveRDS(haw.b.fin, file = "output/biophysical models by province/haw.b.fin.rds")
#====