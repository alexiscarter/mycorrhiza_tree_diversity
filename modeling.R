## Script for the modeling of mycorrhizal proportion

##
## Load packages ####
##

library(tidyverse)
library(brms); options(mc.cores = parallel::detectCores())

##
## Models ####
##

#usa_map_clim object obtained from manipulation.R

## EcM proportion vs q0
mod.q0.ecm <- brm(formula = div.q0 | trunc(lb = 1) ~ prop.ECM.sc + I(prop.ECM.sc^2) + (1|ECOREG),
                            data = usa_map_clim, family=poisson(),
                            warmup = 1000, iter = 5000, chains = 4, thin = 10)
## EcM proportion vs q1
mod.q1.ecm <- brm(formula = div.q1 | trunc(lb = 1) ~ prop.ECM.sc + I(prop.ECM.sc^2) + (1|ECOREG),
                            data = usa_map_clim, family=Gamma(link="log"),
                            warmup = 1000, iter = 5000, chains = 4, thin = 10)
## EcM proportion vs q2
mod.q2.ecm <- brm(formula = div.q2 | trunc(lb = 1) ~ prop.ECM.sc + I(prop.ECM.sc^2) + (1|ECOREG),
                            data = usa_map_clim, family=Gamma(link="log"),
                            warmup = 1000, iter = 5000, chains = 4, thin = 10)

## EcM proportion vs q0 and other predictors
mod.q0.full <- brm(formula = div.q0 | trunc(lb = 1) ~ (prop.ECM.sc + I(prop.ECM.sc^2))*moist + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*ELEV.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*PPT.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*TMEAN.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*SLOPE.sc + (1|ECOREG),
                   data = usa_map_clim, family = poisson(),
                   warmup = 1000, iter = 5000, chains = 4, thin = 10)

## EcM proportion vs q1 and other predictors
mod.q1.full <- brm(formula = div.q1 | trunc(lb = 1) ~ (prop.ECM.sc + I(prop.ECM.sc^2))*moist + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*ELEV.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*PPT.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*TMEAN.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*SLOPE.sc + (1|ECOREG),
                   data = usa_map_clim, family=Gamma(link="log"), inits = 0,
                   warmup = 1000, iter = 5000, chains = 4, thin = 10)

## EcM proportion vs q2 and other predictors
mod.q2.full <- brm(formula = div.q2 | trunc(lb = 1) ~ (prop.ECM.sc + I(prop.ECM.sc^2))*moist + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*ELEV.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*PPT.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*TMEAN.sc + 
                     (prop.ECM.sc + I(prop.ECM.sc^2))*SLOPE.sc + (1|ECOREG),
                   data = usa_map_clim, family=Gamma(link="log"), inits = 0,
                   warmup = 1000, iter = 5000, chains = 4, thin = 10)

## Notes: 
## For AM proportion replace prop.ECM.sc by prop.AM.sc
## For analyses with suplot-level data, species-level data, rarefy data, different minimum number of individuals modify objects as explained in manipulation.R script in order to obtain specific usa_map_clim object. 
