# quick script to clean up the raw gbif download data for lepidoptera
# and subset down to just butterflies that could potentially be used for analyses
# This script will not run in this repository as the raw data from GBIF are not available here
# But they can be downloaded from this doi: https://doi.org/10.15468/dl.p5nhwh

# packages
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(concaveman)

# read in originally downloaded data from GBIF
# download was for all lepidoptera in Germany
# doi: 10.15468/dl.p5nhwh
occurrence <- read_delim("Data/gbif_raw_europe/occurrence.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# get list of potential families to consider
families <- read_csv("Data/traits/coded_traits.csv") %>%
  dplyr::select(Family) %>%
  distinct() %>%
  .$Family

# trim columns and data
# this can be helpful for methods presenting
# information on the data and where the data came from etc.
dat <- occurrence %>%
  dplyr::select(1, 43, 60:62, 64, 68, 69, 71, 99, 103:105, 121, 122,
                133:135, 183, 191:200, 204, 218, 219, 229:233) %>%
  dplyr::filter(hasCoordinate=="TRUE") %>%
  dplyr::filter(hasGeospatialIssues=="FALSE") %>%
  dplyr::select(-hasCoordinate, -hasGeospatialIssues) %>%
  dplyr::filter(family %in% families) %>%
  replace_na(list(coordinateUncertaintyInMeters=0)) %>%
  dplyr::filter(coordinateUncertaintyInMeters<500) %>%
  dplyr::filter(countryCode %in% c("AD", "AL", "AT", "BA", "BE", "BG", 
                                   "CH", "CZ", "DE", "DK", "ES", "FI",
                                   "FR", "GB", "GI", "GR", "HR", "HU",
                                   "IE", "IT", "LI", "LU", "MC", "ME",
                                   "MK", "MT", "NO", "PL", "PT", "RS",
                                   "SE", "SI", "SK", "SM", "XK")) %>%
  group_by(eventDate, decimalLatitude, decimalLongitude, species) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(keep=ifelse(countryCode=="ES" & decimalLatitude < 35.77076357686433, "exclude", "keep")) %>%
  mutate(keep2=ifelse(countryCode=="PT" & decimalLongitude < -9.770280153629612, "exclude", "keep")) %>%
  dplyr::filter(keep=="keep") %>%
  dplyr::filter(keep2=="keep") %>%
  dplyr::select(-keep, -keep2)

# trim again just to necessary data to be used later
export_dat <- dat %>%
  dplyr::select(1, 10, 14, 16:18, 24, 32) %>%
  mutate(ID=1:nrow(.))

saveRDS(export_dat, "Data/gbif_trimmed_europe/gbif_data.RDS")

# turn data into a shapefile for import into GEE
# to get stuff for it
dat_sf <- export_dat %>%
  mutate(ID=1:nrow(.)) %>%
  dplyr::select(9, 4, 5) %>%
  st_as_sf(coords=c("decimalLongitude", "decimalLatitude"))

write_sf(dat_sf, "Data/gbif_dat_shapefile/gbif_dat_spatial.shp")

# create a convex hull of the points
# which will be used to make a grid of to collate points for occupancy models
##ch <- st_convex_hull(st_union(dat_sf))

ch <- concaveman(dat_sf) %>%
  st_set_crs(4326)

st_write(ch, "Data/study_extent_shapefile/study_extent.geojson")
st_write(ch, "Data/study_extent_shapefile/study_extent.shp")

ch2 <- ch %>%
  st_transform(23038)

grids_5 <- st_make_grid(ch2, square = T, cellsize = c(5000, 5000)) %>% # the grid, covering bounding box
  st_sf() %>%
  st_transform(4326) %>%
  mutate(grid_id=1:nrow(.))

grids_5.2 <- grids_5 %>%
  st_intersects(ch) %>%
  as.data.frame() %>%
  rename(grid_id=row.id)

grids_5_trimmed <- grids_5 %>%
  dplyr::filter(grid_id %in% grids_5.2$grid_id)

st_write(grids_5_trimmed, "Data/study_extent_shapefile/study_extent_grids_5k.geojson")
st_write(grids_5_trimmed, "Data/study_extent_shapefile/study_extent_grids_5k.shp")

