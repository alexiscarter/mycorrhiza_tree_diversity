## Script for data manipulation

##
## Load packages ####
##

library(tidyverse)
library(plyr)
library(vegan)
library(readxl)
library(raster)
library(prism)
library(sf)
library(reshape)

##
## Data processing ####
##

## From import.R
# Tree (TREE) data: usa_tree
# Condition (COND) table: usa_cond
# Plot (PLOT) table: usa_plot

## Tree mycorrhizal associations table
sp <- read_excel("data_tree_sp_myc_type.xlsx", col_types = "text")

##
## PLOT table ####
##

usa_plot2 <- usa_plot %>%
  dplyr::filter(!CN %in% PREV_PLT_CN,
                KINDCD %in% c(1,2,3),
                QA_STATUS == "1",
                as.numeric(MANUAL) >= 1,
                SAMP_METHOD_CD == "1",
                INVYR != "9999",
                DESIGNCD == "1") %>%
  dplyr::rename(PLT_CN = CN)

##
## COND table ####
##

## Exclude all plots which do not meet criteria
usa_cond <- usa_plot2 %>% 
  left_join(usa_cond, by = "PLT_CN")

usa_cond2 <- usa_cond %>%
  dplyr::filter(!STATECD.x %in% c("2", "15", "60", "66", "69", "72", "74", "78"),
                COND_STATUS_CD == "1",
                STDORGCD == "0",
                DSTRBCD1 == "0",
                DSTRBCD2 == "0",
                DSTRBCD3 == "0",
                TRTCD1 == "0",
                TRTCD2 == "0",
                TRTCD3 == "0",
                !AFFORESTATION_CD == "1",
                !PREV_AFFORESTATION_CD == "1",
                PRESNFCD == "",
                !ECOSUBCD == "")

## Remove plots where (at least part of) the condition was not adequate
excl.plot <- function(x) {
  if (nrow(x) > 1 && sum(x$CONDPROP_UNADJ) < 1) {x <- x[-1*(1:nrow(x) ) ,]} # remove that plot altogether
  return(x)
}

usa_cond3 <- usa_cond2 %>%
  group_by(PLT_CN) %>%
  dplyr::do(excl.plot(.)) %>%
  dplyr::distinct(PLT_CN, .keep_all = T)

##
## TREE table ####
##

## Select only plots that meet criteria in tree record table
usa_tree2 <- usa_tree %>%
  dplyr::semi_join(usa_cond3, by = 'PLT_CN') %>% 
  dplyr::filter(STATUSCD == "1")

## Remove plots where mycorrhizal type is not known or when TPA_UNADJ is 0 or NA
tmp <- usa_tree2 %>%
  dplyr::left_join(sp, by = 'SPCD', copy = TRUE) %>%
  dplyr::filter(is.na(MycorrhizalType) | is.na(TPA_UNADJ) | TPA_UNADJ == 0) %>%
  dplyr::distinct(PLT_CN)

# Extract plots
usa_tree3 <- usa_tree2 %>%
  dplyr::anti_join(tmp, by = 'PLT_CN')

##
## Calculate basal area ####
##

## Calculate basal area per tree
usa_ba <- usa_tree3 %>%
  inner_join(sp, by = 'SPCD') %>% # merge with sp
  dplyr::select(CN, PLT_CN, SPCD, SpeciesName, MycorrhizalType, DIA, TPA_UNADJ) %>%
  filter(!is.na(DIA)) %>%
  filter(TPA_UNADJ == 6.018046) %>%
  mutate(
    diam = (DIA * 2.54) / 100, # convert in to m
    ba = ((pi * (diam / 2)^2 ) * TPA_UNADJ ) / 0.404686 # calculate basal area per hectare (1 acre = 0.404686 ha)
  )

## Verify structure
str(usa_ba)
usa_ba$PLT_CN <- as.factor(usa_ba$PLT_CN)

## Calculate basal area per species per plot
usa_ba_sp <- usa_ba %>%
  group_by(PLT_CN, SpeciesName) %>% # group by plot and species
  dplyr::summarise(ba.sum = sum(ba, na.rm = T)) # sum basal area per plot and species

## Calculate total basal area per mycorrhizal type per plot
usa_ba_strat <- usa_ba %>%
  group_by(PLT_CN, MycorrhizalType) %>% # group by plot and mycorrhizal type
  dplyr::summarise(ba.sum = sum(ba, na.rm = T)) # sum basal area per plot and mycorrhizal type

## Create matrix of plots by species, with basal area in cells. Convert 'long' format to 'wide format and fill empty cells with 0
usa_ba_sp_mat <- tidyr::spread(usa_ba_sp, SpeciesName, ba.sum, fill = 0)

## Add row names
usa_ba_sp_mat <- usa_ba_sp_mat %>%
  tibble::column_to_rownames('PLT_CN')

## Remove species with total basal area of 0 (if any, potential mistakes).
which.sp <- which(colSums(usa_ba_sp_mat) > 0)
which.sp2 <- which(is.na(colSums(usa_ba_sp_mat)))
usa_ba_sp_mat <- usa_ba_sp_mat[, which.sp]

## Remove plots with total basal area of 0 (if any, potential mistakes).
which.plot <- which(rowSums(usa_ba_sp_mat) > 0 ) 
usa_ba_sp_mat <- usa_ba_sp_mat[which.plot, ]

##
## Calculate tree diversity ####
##

## Transform data with number of individual
usa_ba$pres <- if_else(usa_ba$ba > 0, 1, 0)
usa_pres_mat <- usa_ba %>% 
  dplyr::select(PLT_CN, SpeciesName, pres) %>% 
  dplyr::group_by(PLT_CN, SpeciesName) %>% 
  dplyr::count() %>% 
  pivot_wider(names_from = SpeciesName, values_from = n, values_fill = list(n = 0)) %>% 
  column_to_rownames('PLT_CN')

## Remove plots with less than X individuals
usa_pres_mat_thres <- usa_pres_mat[rowSums(usa_pres_mat) > 4,] # X = 5

# Diversity values
usa_div <- data.frame(div.q0 = specnumber(usa_pres_mat_thres),
                      div.q1 = exp(diversity(usa_pres_mat_thres, index = "shannon")), 
                      div.q2 = diversity(usa_pres_mat_thres, index = "invsimpson"))

## Add plot name
usa_div$PLT_CN <- as.factor(rownames(usa_div))

## Function to calculate proportion of basal area
calc.ba <- function(x) { 
  AM <- ifelse(any(x$MycorrhizalType == 'AM'), sum(x$ba.sum[x$MycorrhizalType == 'AM']), 0) 
  ECM <- ifelse(any(x$MycorrhizalType == 'EM'), sum(x$ba.sum[x$MycorrhizalType == 'EM']), 0) 
  Either <- ifelse(any(x$MycorrhizalType == 'AM+EM'), sum(x$ba.sum[x$MycorrhizalType == 'AM+EM']), 0)
  NM <- ifelse(any(x$MycorrhizalType == 'NM'), x$ba.sum[x$MycorrhizalType == 'NM'], 0) 
  ERM <- ifelse(any(x$MycorrhizalType == 'ERM'), x$ba.sum[x$MycorrhizalType == 'ERM'], 0) 
  AM <- AM + (Either / 2)
  ECM <- ECM + (Either / 2)
  total <- sum(x$ba.sum, na.rm = T) 
  prop.AM <- AM / total
  prop.ECM <- ECM / total
  prop.ERM <- ERM / total
  prop.NM <- NM / total
  prop.dual <- Either / total
  res <- data.frame(prop.AM, prop.ECM, prop.ERM, prop.NM, prop.dual)
  return(res) 
} 

## Calculate proportion of basal area as AM, EcM, etc.
usa_ba_strat$MycorrhizalType <- as.factor(usa_ba_strat$MycorrhizalType)
usa_ba_am <- ddply(usa_ba_strat, .(PLT_CN), calc.ba)

## Merge basal area data data with diversity data
usa_df <- merge(usa_ba_am, usa_div, by = 'PLT_CN')

## Moisture characterization. Add PHYSCLCD conditions in usa_df table
usa_df <- usa_df %>%
  inner_join(dplyr::select(usa_cond3, PLT_CN, PHYSCLCD), by = 'PLT_CN')

## Create new factor to classify plots into xeric, mesic or hydric sites
usa_df$moist <- factor(NA, levels = c('Xeric', 'Mesic', 'Humid'))

## Sort plots depending on the moisture available for the trees
usa_df$moist[usa_df$PHYSCLCD %in% c(11, 12, 13, 19)] <- 'Xeric' # only xeric sites
usa_df$moist[usa_df$PHYSCLCD %in% c(21, 22, 23, 24, 25, 29)] <- 'Mesic' # only mesic sites
usa_df$moist[usa_df$PHYSCLCD %in% c(31, 32, 33, 34, 35, 39)] <- 'Humid' # only hydric sites

## Add ecological region information
usa_map <- left_join(usa_df, usa_cond3) %>% 
  dplyr::filter(!is.na(moist))

usa_map$ECOSUB = gsub("[A-Za ]", "", usa_map$ECOSUBCD)
usa_map$ECOREG = gsub("[A-Za-z ]", "", usa_map$ECOSUBCD)
usa_map$ECODIV = substr(usa_map$ECOREG, 1, 2)

##
## Add climate data from PRISM project ####
##

## Convert raster data to point data
new_file1<-2 # this number corresponds to the row of the file of interest, check with ls_prism_data()[1:10,]
new_file2<-4 # same here than above

## Raster file
RS.1 <- prism_stack(ls_prism_data()[new_file1,1])
RS.2 <- prism_stack(ls_prism_data()[new_file2,1])

## Projection
proj4string(RS.1)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
proj4string(RS.2)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## Convert raster to point dataframe
df1 <- data.frame(rasterToPoints(RS.1))
df2 <- data.frame(rasterToPoints(RS.2))
m.df1 <- melt(df1, c("x", "y"))
m.df2 <- melt(df2, c("x", "y"))

## Rename columns
names(m.df1)[1:2] <- c("lon", "lat")
names(m.df2)[1:2] <- c("lon", "lat")

## Add precipitation and temperature for each FIA plot
usa_map_spdf<-SpatialPointsDataFrame(coords=usa_map[,c('LON','LAT')], 
                                     data=usa_map, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
usa_map_spdf_clim<-extract(RS.1, usa_map_spdf,  fun=mean, na.rm=TRUE, sp=TRUE)
usa_map_spdf_clim<-extract(RS.2, usa_map_spdf_clim,  fun=mean, na.rm=TRUE, sp=TRUE)
usa_map_clim <- as.data.frame(usa_map_spdf_clim)

## Remove missing plot and rename columns
usa_map_clim <- usa_map_clim %>%
  filter(!PLT_CN == '104967932010661') %>% # no values, probably because it is located in an island in lake Huron
  dplyr::rename("PPT" = "PRISM_ppt_30yr_normal_800mM2_annual_bil") %>% 
  dplyr::rename("TMEAN" = "PRISM_tmean_30yr_normal_800mM2_annual_bil")

## Scaling
usa_map_clim$prop.ECM.sc <- scale(usa_map_clim$prop.ECM)
usa_map_clim$prop.AM.sc <- scale(usa_map_clim$prop.AM) 
usa_map_clim$ELEV.sc <- scale(usa_map_clim$ELEV)
usa_map_clim$PPT.sc <- scale(usa_map_clim$PPT)
usa_map_clim$PPT.sc <- scale(usa_map_clim$PPT)
usa_map_clim$TMEAN.sc <- scale(usa_map_clim$TMEAN)
usa_map_clim$SLOPE.sc <- scale(usa_map_clim$SLOPE)

## Note: usa_map_clim object is used to make, exploration figures, maps and models.

##
## Extra manipulation before modeling for secondary analyses ####
##

## To only use tree mycorrhizal types at the species-level
## Modify sp object by:
sp <- sp %>% filter(IDlevel == "species")

## To remove plots with less than X individuals
## Modify usa_pres_mat_thres object by:
usa_pres_mat_thres <- usa_pres_mat[rowSums(usa_pres_mat) > 9,] # X = 10
usa_pres_mat_thres <- usa_pres_mat[rowSums(usa_pres_mat) > 14,] # X = 15

## To calculate diversity based on rarefy data
## Modify usa_div object by:
usa_div <- data.frame(div.q0 = specnumber(usa_pres_mat_thres9),
                      div.raref = rarefy(usa_pres_mat_thres9, 10))

## To do analyses at the subplot-level
## Modify the following objects:
usa_ba <- usa_tree3 %>%
  inner_join(sp, by = 'SPCD') %>% # merge with sp
  dplyr::select(CN, PLT_CN, SUBP, SPCD, SpeciesName, MycorrhizalType, DIA, TPA_UNADJ) %>% # SUBP (subplot) here
  filter(!is.na(DIA)) %>%
  filter(TPA_UNADJ == 6.018046) %>%
  mutate(
    diam = (DIA * 2.54) / 100,
    ba = ((pi * (diam / 2)^2 ) * TPA_UNADJ ) / 0.404686
  )

usa_ba_sp <- usa_ba %>%
  group_by(PLT_CN, SUBP, SpeciesName) %>% # SUBP here
  dplyr::summarise(ba.sum = sum(ba, na.rm = T))

usa_ba_strat <- usa_ba %>%
  group_by(PLT_CN, SUBP, MycorrhizalType) %>% # SUBP here
  dplyr::summarise(ba.sum = sum(ba, na.rm = T))

