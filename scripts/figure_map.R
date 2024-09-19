#Map
# Fri Dec  1 15:49:33 2023 ------------------------------
library(tidyverse)
library(emmeans)
library(marmap)
library(rnaturalearth)
library(raster)
values <- c("#14c4ac", "#fc944c", "#8c54fc", "#fc94e4", "#7cdc54", "#fcdc54", "#cc9c44")

dat <- readRDS("data/dat.Rda")

prov <- levels(droplevels(filter(dat, lat >0, PROVINCE != "Mediterranean Sea")$PROVINCE))

#make base map
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180,#query bathymetric data using marmap package
                       lat1 = -90, lat2 = 90, resolution = 30, keep = TRUE)

bathy.f <- fortify(bathy)
north <- filter(map_data("world"), lat>0)
world <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')


fortify(r.meow)

library(ggnewscale)

jpeg("map.jpg", units="in", width=20, height=12, res=300)

ggplot(bathy.f, aes(x = x, y = y)) + 
  coord_quickmap() + 
  geom_raster(aes(fill = z), data = bathy.f[bathy.f$z<=0,]) + 
  geom_contour(aes(z = z), breaks = c(-500, -2000), color = "white", linewidth = 0.1) +
  geom_sf(data = world, inherit.aes = FALSE, fill = "#222021") +
  guides(fill = "none") +
  new_scale_fill() + 
  # geom_map(
  #   data = north, map = north,
  #   aes(long, lat, map_id = region))  +
  geom_sf(data = filter(meow["PROVINCE"], PROVINCE %in% prov), aes(fill = PROVINCE), color = "white", linewidth = .75, alpha = 0.4, inherit.aes = FALSE) + 
  geom_point(data = distinct(dplyr::select(filter(dat, lat >0, PROVINCE != "Mediterranean Sea"), site, PROVINCE, lat, lon)), aes(lon, lat, fill = PROVINCE), size = 10, pch = 21, color = "black") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_color_manual(values = values) + 
  scale_fill_manual(values = values) + 
  xlim(-190, 190) +
  ylim(0, 90) +
  guides(fill = "none")

dev.off()

#ICELAND OSM MAP
ice.bathy <- getNOAA.bathy(lon1 = -34, lon2 = -4,#query bathymetric data using marmap package
                       lat1 = 56, lat2 = 76, resolution = 4, keep = TRUE)

ice.bathy.f <- fortify(ice.bathy)

jpeg("map_Iceland.jpg", units="in", width=20, height=12, res=300)

ggplot(ice.bathy.f, aes(x = x, y = y)) + 
  coord_quickmap() + 
  geom_raster(aes(fill = z), data = ice.bathy.f[ice.bathy.f$z<=0,]) + 
  geom_contour(aes(z = z), breaks = c(-500, -2000), color = "white", linewidth = 0.1) +
  guides(fill = "none") +
  new_scale_fill() + 
  # geom_map(
  #   data = north, map = north,
  #   aes(long, lat, map_id = region))  +
  geom_sf(data = filter(meow["ECOREGION"], ECOREGION %in% c("North and East Iceland", "South and West Iceland")), aes(fill = ECOREGION), alpha = 0.6, inherit.aes = FALSE) + 
  geom_sf(data = world, inherit.aes = FALSE, fill = "lightgrey") +
  geom_point(data = distinct(dplyr::select(filter(dat, lat >0, PROVINCE != "Mediterranean Sea"), site, ECOREGION, lat, lon)), aes(lon, lat, fill = ECOREGION), size = 10, pch = 21, color = "white") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  xlim(-34, -4) +
  ylim(56, 76) +
  guides(fill = "none")

dev.off()


#For OSM Map
premier <- readxl::read_xlsx("output/premier_sites.xlsx")

jpeg("map_OSM_premier.jpg", units="in", width=20, height=12, res=300)

ggplot(bathy.f, aes(x = x, y = y)) + 
  coord_quickmap() + 
  geom_raster(aes(fill = z), data = bathy.f[bathy.f$z<=0,]) + 
  geom_contour(aes(z = z), breaks = c(-500, -2000), color = "white", linewidth = 0.1) +
  geom_sf(data = world, inherit.aes = FALSE, fill = "lightgrey") +
  guides(fill = "none") +
  new_scale_fill() + 
  # geom_map(
  #   data = north, map = north,
  #   aes(long, lat, map_id = region))  +
  #geom_sf(data = filter(meow["PROVINCE"], PROVINCE %in% prov), aes(fill = PROVINCE), alpha = 0.6, inherit.aes = FALSE) + 
  geom_point(data = distinct(select(dat, lat, lon)), aes(lon, lat), size = 10, pch = 21, color = "white", fill = "black", alpha = 0.8) +
  geom_point(data = filter(distinct(select(dat, lat, lon, site)), site %in% filter(premier, series_per_site>3)$site), aes(lon, lat), size = 10, pch = 21, color = "white", fill = "red") +#add premier sites
  theme_void() +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_color_manual(values = values) + 
  scale_fill_manual(values = values) + 
  guides(fill = "none")

dev.off()



ggplot(ras.df) +
  geom_raster(aes(x=x,y=y,fill = factor(PROVINCE)))

plot(r.meow)
ras.df <- as.data.frame(meow, xy = TRUE) %>% drop_na()
str(ras.df)
head(ras.df)

plot(st_geometry(meow))
plot(meow["PROVINCE"])

ggplot() + 
  geom_sf(data = filter(meow["PROVINCE"], PROVINCE %in% prov), aes(fill = PROVINCE), alpha = 0.3) + 
  scale_fill_manual(values = values)
