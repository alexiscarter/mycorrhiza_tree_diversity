## Script to make exploration figures, maps and post-modeling figures

##
## Load packages ####
##

library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(tidybayes)
library(RColorBrewer)

##
## Exploration ####
##

## AM vs EcM proportion
ggplot(usa_map_clim, aes(x = prop.ECM, y = prop.AM)) +
  geom_bin2d(bins=20) +
  scale_fill_gradientn(name = "Number\nof plots", colours=rev(c("#ff0000", "#ff9e75", "#fff4f0")), trans="log10", n.breaks = 6) +
  labs(x='EcM proportion', y = 'AM proportion') +
  scale_x_continuous( expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() + theme(text = element_text(family = 'Helvetica', colour = "black", size = 7))

## Histogram with number of plots vs EcM dominance
ecmprop_nplot <- ggplot_build(
  ggplot() + geom_histogram(aes(x = usa_map_clim$prop.ECM, fill = usa_map_clim$prop.ECM), binwidth = 0.01)) # Get number of bin/colours 
nu_bins <- dim(ecmprop_nplot$data[[1]])[1]

ggplot(data = usa_map_clim, aes(x = prop.ECM, fill = ..x..)) + 
  geom_histogram(binwidth = 0.01, color = 'black', size = 0.3, boundary = 0) +
  scale_fill_viridis_c(direction = -1, minor_breaks = nu_bins) +
  labs(x = 'EcM proportion', y = "Number of plots") +
  scale_x_continuous(expand = c(0.005, 0.001)) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, 1000), expand = c(0, 0)) +
  theme_classic() + theme(text = element_text(family = 'Helvetica', colour = "black"), legend.position = 'none',
                          panel.grid.major.y = element_line(colour = "black", size = .1),
                          panel.grid.minor.y = element_line(colour = "black", size = .05),
                          axis.line = element_line(colour = "black",size = 1))

## Histogram with number of plots vs AM dominance
amprop_nplot <- ggplot_build(
  ggplot() + geom_histogram(aes(x = usa_map_clim$prop.AM, fill = usa_map_clim$prop.AM), binwidth = 0.01))
nu_bins <- dim(amprop_nplot$data[[1]])[1]

ggplot(data = usa_map_clim, aes(x = prop.AM, fill = ..x..)) + 
  geom_histogram(binwidth = 0.01, color = 'black', size = 0.3, boundary = 0) +
  scale_fill_viridis_c(direction = -1, minor_breaks = nu_bins) +
  labs(x = 'AM proportion', y = "Number of plots") +
  scale_x_continuous(expand = c(0.005, 0.001)) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, 1000), expand = c(0, 0)) +
  theme_classic() + theme(text = element_text(family = 'Helvetica', colour = "black"), legend.position = 'none',
                          panel.grid.major.y = element_line(colour = "black", size = .1),
                          panel.grid.minor.y = element_line(colour = "black", size = .05),
                          axis.line = element_line(colour = "black",size = 1))

##
## Maps ####
##

US <- ne_countries(country = c('United states of America', 'Canada', 'Mexico'), scale = "medium", returnclass = "sf") # Load geographic information

## Ecological region
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

ggplot(data = US) +
  geom_sf(color = "darkgrey", fill = "white") +
  coord_sf(xlim = c(-125, -64.5), ylim = c(24, 50), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = usa_map_clim, mapping = aes(x = LON, y = LAT, color = ECOREG), size = 1.5, alpha = 1) + # Coordinates of the plots
  scale_color_manual(name = "Ecoregions", values = col_vector)+
  labs(x = 'Longitude (°)', y = 'Latitude (°)') +
  theme(axis.ticks = element_line(size = .3), panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(), panel.background = element_rect(fill = 'aliceblue'), text = element_text(family = 'Helvetica', size = 7))

# Tree richness
ggplot(data = US) +
  geom_sf(color = "darkgrey", fill = "white", size = .3) +
  coord_sf(xlim = c(-125, -64.5), ylim = c(24, 50), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), style = north_arrow_fancy_orienteering) +
  geom_point(data = usa_map_clim, mapping = aes(x = LON, y = LAT, color = div.q0), shape = 16, size = .2, alpha = .6) +
  scale_color_viridis_c(name = 'Tree\nrichness', direction = -1) +
  labs(x = 'Longitude (°)', y = 'Latitude (°)') +
  theme(axis.ticks = element_line(size = .3), panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(), panel.background = element_rect(fill = 'aliceblue'), text = element_text(family = 'Helvetica', size = 7))

# EcM proportion
ggplot(data = US) +
  geom_sf(color = "darkgrey", fill = "white", size = .3) +
  coord_sf(xlim = c(-125, -64.5), ylim = c(24, 50), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), style = north_arrow_fancy_orienteering) +
  geom_point(data = usa_map_clim, mapping = aes(x = LON, y = LAT, color = prop.ECM), shape = 16, size = .2, alpha = .6) +
  scale_color_viridis_c(name = 'EcM\nproportion', direction = -1) +
  labs(x = 'Longitude (°)', y = 'Latitude (°)') +
  theme(axis.ticks = element_line(size = .3), panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(), panel.background = element_rect(fill = 'aliceblue'), text = element_text(family = 'Helvetica', size = 7))

# AM proportion
ggplot(data = US) +
  geom_sf(color = "darkgrey", fill = "white", size = .3) +
  coord_sf(xlim = c(-125, -64.5), ylim = c(24, 50), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), style = north_arrow_fancy_orienteering) +
  geom_point(data = usa_map_clim, mapping = aes(x = LON, y = LAT, color = prop.AM), shape = 16, size = .2, alpha = .6) +
  scale_color_viridis_c(name = 'AM\nproportion', direction = -1) +
  labs(x = 'Longitude (°)', y = 'Latitude (°)') +
  theme(axis.ticks = element_line(size = .3), panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(), panel.background = element_rect(fill = 'aliceblue'), text = element_text(family = 'Helvetica', size = 7))

# Dual-AM/EcM tree proportion
ggplot(data = US) +
  geom_sf(color = "darkgrey", fill = "white") +
  coord_sf(xlim = c(-125, -64.5), ylim = c(24, 50), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"), style = north_arrow_fancy_orienteering) +
  geom_point(data = usa_map, mapping = aes(x = LON, y = LAT, color = prop.dual), size = .3, alpha = .8) +
  scale_color_gradient(name = 'Dual-AM/EcM\ntree proportion', low = 'lightgrey', high = 'blue')+
  labs(x = 'Longitude (°)', y = 'Latitude (°)') +
  theme(axis.ticks = element_line(size = .3), panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(), panel.background = element_rect(fill = 'aliceblue'), text = element_text(family = 'Helvetica', size = 7))

##
## Post-modeling ####
##

## Extract data from model output
marg<-conditional_effects(MODEL.OUTPUT) # replace MODEL.OUTPUT by the actual model output you want to plot
marg.dat<-marg$prop.ECM.sc # extracts EcM proportion

## Relationships between mycorrhizal proportion vs diversity
ggplot(usa_map_clim, aes(x = prop.ECM, y = div.q0)) +
  geom_bin2d(bins=17, alpha = 1) +
  scale_fill_gradientn(name = "Number\nof plots", colours=rev(c("#ff0000", "#ff9e75", "#fff4f0")), trans="log10", n.breaks = 6) +
  labs(x='EcM or AM proportion', y = 'Diversity') +
  scale_y_continuous(breaks = seq(1, 100, 1), expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0)) +
  geom_line(data = marg.dat, aes(x= prop.ECM.sc*attr(usa_map_clim$prop.ECM.sc, 'scaled:scale') + attr(usa_map_clim$prop.ECM.sc, 'scaled:center')  ,y=estimate__),size=1, color="black")+
  geom_ribbon(data = marg.dat, aes(x= prop.ECM.sc*attr(usa_map_clim$prop.ECM.sc, 'scaled:scale') + attr(usa_map_clim$prop.ECM.sc, 'scaled:center') ,y=estimate__, ymax=upper__, ymin=lower__),fill="skyblue4",alpha=0.3)+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black",size = .6), axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = 'Helvetica', colour = "black"),
        text = element_text(family = 'Helvetica', colour = "black", size = 7))

## For coefficients
table.mod <- MODEL.OUTPUT %>% # replace MODEL.OUTPUT by the actual model output you want to plot
  gather_draws(b_prop.ECM.sc, b_Iprop.ECM.scE2, b_moistMesic, b_moistHumid, b_ELEV.sc,
               b_PPT.sc, b_TMEAN.sc, b_ELEV.sc, b_PPT.sc, b_TMEAN.sc, b_SLOPE.sc,
               `b_prop.ECM.sc:moistHumid`, `b_prop.ECM.sc:moistMesic`, `b_Iprop.ECM.scE2:moistMesic`, `b_Iprop.ECM.scE2:moistHumid`, `b_prop.ECM.sc:ELEV.sc`, `b_Iprop.ECM.scE2:ELEV.sc`, `b_prop.ECM.sc:PPT.sc`, `b_Iprop.ECM.scE2:PPT.sc`, `b_prop.ECM.sc:TMEAN.sc`, `b_Iprop.ECM.scE2:TMEAN.sc`, `b_prop.ECM.sc:SLOPE.sc`, `b_Iprop.ECM.scE2:SLOPE.sc`) %>% 
  median_qi %>% 
  print(n=30)

## Rename and choose lines (need to be re-coded depending on model output)
table.mod.q0.full$.variable <- c("Elevation", "EcM proportion (second order)", "EcM proportion (second order):Elevation",
                                 "EcM proportion (second order):Humid", "EcM proportion (second order):Mesic",
                                 "EcM proportion (second order):Precipitation", "EcM proportion (second order):Slope", "EcM proportion (second order):Temperature",
                                 "Physiography (humid)", "Physiography (mesic)", "Annual precipitation", "EcM proportion (first order)", "EcM proportion (first order):Elevation",
                                 "EcM proportion (first order):Humid", "EcM proportion (first order):Mesic",
                                 "EcM proportion (first order):Precipitation", "EcM proportion (first order):Slope", "EcM proportion (first order):Temperature",
                                 "Slope", "Mean annual temperature")
table.mod.q0.full <- table.mod.q0.full[-c(4,5,9,10,14,15),]
table.mod.q0.full$.variable <- factor(table.mod.q0.full$.variable , levels = table.mod.q0.full$.variable [order(abs(table.mod.q0.full$.value))])

## Plot
ggplot(table.mod.q0.full, aes(y = .variable, x = .value, xmin = .lower, xmax = .upper)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "black", size=.3) +
  scale_x_continuous(limits = c(-.470,.080), n.breaks = 7) +
  labs(x = "Posterior coefficient estimates", y = "") +
  geom_pointrange(size=.6, color="black", fill="white", shape= 1, stroke = .4, fatten = 2) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black",size = .4), axis.ticks = element_line(colour = "black", size = .4),
        panel.grid.major = element_line(colour = "black", size = .001), panel.grid.minor = element_line(colour = "black", size = .001),
        axis.text = element_text(family = 'Helvetica', colour = "black"),
        axis.title.y=element_blank(),
        text = element_text(family = 'Helvetica', colour = "black", size = 5))
