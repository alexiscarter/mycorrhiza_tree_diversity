## Script to formulate a null model to compare evaluate how local tree diversity is influenced by regional patterns

##
## Load packages ##
##

library(reshape)
library(dplyr)
library(readxl)
library(vegan)
library(tidyverse)
library(brms); options(mc.cores = parallel::detectCores())

##
## Data processing ####
##

# Note: use usa_tree3 and usa_cond3 objects obtained from manipulation.R

## Tree mycorrhizal associations table
sp <- read_excel("data_tree_sp_myc_type.xlsx", col_types = "text")

## Calculate basal area per tree
usa_ba <- usa_tree3 %>%
  inner_join(sp, by = 'SPCD') %>%
  left_join(usa_cond3, by = 'PLT_CN') %>%
  select(PLT_CN, SPCD, SpeciesName, MycorrhizalType, DIA, TPA_UNADJ, ECOSUBCD) %>%
  filter(!is.na(DIA)) %>%
  filter(TPA_UNADJ == 6.018046) %>%
  mutate(
    diam = (DIA * 2.54) / 100,
    ba = ((pi * (diam / 2)^2 ) * TPA_UNADJ ) / 0.404686
  )

## Create ecoregion categories
usa_ba$ECOSUB = gsub("[A-Za ]", "", usa_ba$ECOSUBCD)
usa_ba$ECOREG = gsub("[A-Za-z ]", "", usa_ba$ECOSUBCD)

## Check number of individuals per ecoregion
nsp <- usa_ba %>% 
  dplyr::group_by(ECOREG) %>% 
  dplyr::count()

## Calculate basal area per ecoregion per mycorrhizal type
reg.ba <- usa_ba %>%
  filter(SpeciesName != 'NA') %>%
  filter(ECOREG == XXX) %>% # replace XXX by the number of the ecoregion of interest
  arrange(desc(MycorrhizalType))

## Regional abundance per species
reg.abund.sp <- reg.ba %>%
  dplyr::group_by(SpeciesName) %>%
  dplyr::count()

## Regional basal area per species
reg.ba.sum <- reg.ba %>%
  dplyr::group_by(SpeciesName) %>%
  dplyr::summarize(ba.reg = sum(ba))

## Split data by mycorrhizal types
ECM.pool <- reg.ba %>% 
  dplyr::filter(MycorrhizalType == 'EM') %>% 
  dplyr::group_by(SpeciesName) %>%
  dplyr::count()

notECM.pool <- reg.ba %>% 
  dplyr::filter(MycorrhizalType != 'EM') %>% 
  dplyr::group_by(SpeciesName) %>%
  dplyr::count()

## At the local scale
## Observed total number of individuals in each plot
n.TOT <- reg.ba %>%
  dplyr::group_by(PLT_CN) %>%
  dplyr::count()

## Add observed number of ECM individuals in each plot
n.TOT <- reg.ba %>%
  dplyr::filter(MycorrhizalType == 'EM') %>% 
  dplyr::group_by(PLT_CN) %>%
  dplyr::count() %>% 
  right_join(n.TOT, by = "PLT_CN", suffix = c(".EM", ".TOT")) %>% 
  replace_na(list(n.EM = 0)) 

## Define parameters for the null model
## At the regional scale
## Number of species
num.spp <- length((unique(reg.ba$SpeciesName)))

## Number of species per mycorrhizal type
num.ECM <- nrow(ECM.pool)
num.notECM <- nrow(notECM.pool)

## Number of plots
num.plots <- length((unique(reg.ba$PLT_CN)))

## Number of simulations
num.sim <- 100

## Define output data filled with 0
out.put <- array(0, c(num.spp, num.plots, num.sim))

## Null model loop
## Loop over plots
for (i in 1:num.plots) {
  
  # define a number of individuals to select of each mycorrhizal type
  plot.ECM <- n.TOT$n.EM[i]
  plot.notECM <- n.TOT$n.TOT[i] - n.TOT$n.EM[i]
  
  # if NA created (no species in one type), replace by zero
  plot.ECM[is.na(plot.ECM)] <- 0
  plot.notECM[is.na(plot.notECM)] <- 0
  
  # loop over simulations
  for (j in 1:num.sim) {
    
    # if there are some ECM individuals, take a random selection
    if (plot.ECM > 0) {
      # randomly sample plot.ECM individuals from the species pool
      com.ECM <- sample(1:num.ECM, plot.ECM, prob = ECM.pool$n/sum(ECM.pool$n), replace=TRUE)
      
      # the next two steps organize the simulated data so cast will work
      com.ECM <- as.data.frame(com.ECM)
      com.ECM$count <- 1
      # calculate the number of individuals of each species
      com.out <- com.ECM %>% dplyr::group_by(com.ECM) %>% dplyr::count()
      # record the data in the output
      out.put[com.out$com.ECM, i, j] <- com.out$n
    } 
    
    # repeat for notECM individuals
    if (plot.notECM > 0) {
      com.notECM <- sample(1:num.notECM, plot.notECM, prob = notECM.pool$n/sum(notECM.pool$n), replace=TRUE)
      com.notECM <- as.data.frame(com.notECM)
      com.notECM$count <- 1
      
      # calculate the number of individuals of each species
      com.out2 <- com.notECM %>% dplyr::group_by(com.notECM) %>% dplyr::count()
      
      # to work this requires that spp are listed ECM first then notECM
      out.put[com.out2$com.notECM+num.ECM, i, j] <- com.out2$n
    } 
  }
}

## Calculate diversity for simulated communities
sim.q0 <- apply(X = out.put, MARGIN = 2:3, function(x) specnumber(x))
sim.q1 <- apply(X = out.put, MARGIN = 2:3, function(x) exp(diversity(x, index = "shannon")))
sim.q2 <- apply(X = out.put, MARGIN = 2:3, function(x) diversity(x, index = "invsimpson"))

## Calculate diversity for observed communities
## Transform basal are in presence/absence to count individuals
reg.ba$pres <- if_else(reg.ba$ba > 0, 1, 0)

reg.ba.mat <- reg.ba %>% 
  dplyr::select(-MycorrhizalType) %>% 
  dplyr::group_by(PLT_CN, SpeciesName) %>% 
  dplyr::count() %>% 
  pivot_wider(names_from = SpeciesName, values_from = n, values_fill = list(n = 0)) %>% 
  column_to_rownames('PLT_CN')

obs.q0 <- t(t(apply(X = reg.ba.mat, MARGIN = 1, function(x) specnumber(x))))
obs.q1 <- t(t(apply(X = reg.ba.mat, MARGIN = 1, function(x) exp(diversity(x, index = "shannon")))))
obs.q2 <- t(t(apply(X = reg.ba.mat, MARGIN = 1, function(x) diversity(x, index = "invsimpson"))))

## Calculate Z-scores between observed diversity in plots and simulations
## Custom function  
net.zscore = function(obs.com, null.com) {
  (obs.com - rowMeans(null.com)/apply(null.com, 1, function(x) sd(x)))  
}

## q = 0
Zs <- net.zscore(obs.q0, sim.q0)
## q = 1
Zs <- net.zscore(obs.q1, sim.q1)
## q = 2
Zs <- net.zscore(obs.q2, sim.q2)

## Make a dataframe with Z-scores and corresponding p-values
Ps <- 2*pnorm(-abs(Zs))
Zscore <- cbind(Zs, Ps)
colnames(Zscore) = c('Zscore', 'Pvalue')

tmp.XXX <- cbind(Zscore, rep('XXX', dim(Zscore)[1])) # replace XXX by the number of the ecoregion of interest.

## Run the null model for every ecoregion and then bind the tmp.XXX table created for q0. Same approach is applied for q1 and q2
#zscore.q0 <- rbind(tmp.XXX, tmp.XXX, ...)

zscore.q0 <- zscore.q0 %>% 
  as.data.frame() %>% 
  rownames_to_column('PLT_CN') 

## For easier processing, combine directly with usa_map_clim (from manipulation.R)
zscore.clim.q0 <- usa_map_clim %>%
  left_join(zscore.q0, by = "PLT_CN")

## Modeling
mod.null <- brm(formula = Zscore ~ (prop.ECM.sc + I(prop.ECM.sc^2))*moist +
                        (prop.ECM.sc + I(prop.ECM.sc^2))*ELEV.sc +
                        (prop.ECM.sc + I(prop.ECM.sc^2))*PPT.sc +
                        (prop.ECM.sc + I(prop.ECM.sc^2))*TMEAN.sc +
                        (prop.ECM.sc + I(prop.ECM.sc^2))*SLOPE.sc + (1|ECOREG),
                      data = zscore.clim.q0, family = skew_normal(),
                      warmup = 1000, iter = 5000, chains = 4, thin = 10)

