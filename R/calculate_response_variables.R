# An R script to calculate response variable
# for every butterfly species
# this is the 'simple' response variables
# different to the more complicated
# occurrence model coefficient that is a result of occupancy modelling

# Here there are two types of response variables of urbanness
# One using all observations from a butterfly's range throughout Europe
# And a second accounting for range size to an extent and the available urban habitat
# for a given species

# packages
library(dplyr)
library(sf)
library(tidyr)
library(concaveman)

# read in the different datasets
viirs <- readRDS("Data/night_lights_data/gbif_viirs_lights_export.RDS")

raw_dat <- readRDS("Data/gbif_trimmed_europe/gbif_data.RDS")

points_sf <- st_read("Data/gbif_dat_shapefile/gbif_dat_spatial.shp") %>%
  st_set_crs(4326)

# First calculate the simplest response variables
# where we disregard the range size and take the median of all observations for a species
# will use a cutoff of 250 observations as to whether a species can
# be included in analyses or not
response_variables <- raw_dat %>%
  left_join(., viirs, by="ID") %>%
  group_by(species) %>%
  summarize(N=n(),
            median_lights=median(viirs),
            mean_lights=mean(viirs),
            quantile_75_lights=quantile(viirs, .75),
            quantile_25_lights=quantile(viirs, .25),
            sd_lights=sd(viirs)) %>%
  dplyr::filter(N>250) %>%
  dplyr::filter(complete.cases(species))


# Now calculate the "range-wide" distribution
# for each species separately

get_range_urbanness <- function(species_name) {
  
  # get all the spatial observations
  # for a species from the sf object
  spatial_dat_sp_id <- raw_dat %>%
    dplyr::filter(species==species_name) %>%
    .$ID
  
  spatial_dat_sp <- points_sf %>%
    dplyr::filter(ID %in% spatial_dat_sp_id)
  
  # now make a convex hull range map of this species' range
  ch <- concaveman(spatial_dat_sp) %>%
    st_set_crs(4326)
  
  # then get all of the total observations that fall within that species' range
  range_obs <- ch %>%
    st_intersects(points_sf) %>%
    as.data.frame()
  
  # now get the IDs and the VIIRS measurements of all these observations
  range_dat <- range_obs %>%
    rename(ID=col.id) %>%
    left_join(., viirs, by="ID")
  
  # get the summary df of range dat measures
  summary_df <- data.frame(species=species_name, 
                           range_N=nrow(range_dat),
                           range_median_lights=median(range_dat$viirs),
                           range_mean_lights=mean(range_dat$viirs),
                           range_quantile_75_lights=quantile(range_dat$viirs, .75),
                           range_quantile_25_lights=quantile(range_dat$viirs, .25),
                           range_sd_lights=sd(range_dat$viirs))
  
}

# now run this function for every species in our response variables
range_data_summary <- bind_rows(lapply(response_variables$species, get_range_urbanness))

# add this to the response variables calculated above
response_variables <- response_variables %>%
  left_join(., range_data_summary, by="species")

saveRDS(response_variables, "Results/butterfly_urban_scores/butterfly_urban_scores.RDS")
