# make a figure S1 for supplementary paper

library(rnaturalearth)
library(dplyr)
library(sf)
library(ggplot2)

study_extent <- st_read("Data/study_extent_shapefile/study_extent.geojson")

world <- ne_countries(scale = 'medium', type = 'map_units', returnclass="sf")

europe <- world %>%
  dplyr::filter(continent=="Europe")

head(world[c('name', 'continent')])

ggplot()+
  geom_sf(data=europe, fill="gray90")+
  geom_sf(data=study_extent, fill="gray20", alpha=0.8)+
  theme_bw()+
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggsave("Figures/figure_s1_study_extent_map.png", width=5, height=4, units="in")
