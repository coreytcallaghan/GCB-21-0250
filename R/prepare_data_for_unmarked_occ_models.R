# this script takes the grids and creates a lookup table
# to see whether a species is in a grid or not

# packages
library(sf)
library(dplyr)
library(tidyr)
library(concaveman)

# read in the grids
grids_5 <- st_read("Data/study_extent_shapefile/study_extent_grids_5k.shp")

# read in the different datasets
viirs <- readRDS("Data/night_lights_data/gbif_viirs_lights_export.RDS")

raw_dat <- readRDS("Data/gbif_trimmed_europe/gbif_data.RDS")

points_sf <- st_read("Data/gbif_dat_shapefile/gbif_dat_spatial.shp") %>%
  st_set_crs(4326)

# get a list of species to be considered for analysis
species_list <- readRDS("Results/butterfly_urban_scores/butterfly_urban_scores.RDS") %>%
  .$species

# now for each species get a list of grids that it is in
species_grid_function <- function(species_name) {
  
  # get all the spatial observations
  # for a species from the sf object
  spatial_dat_sp_id <- raw_dat %>%
    dplyr::filter(species==species_name) %>%
    .$ID
  
  spatial_dat_sp <- points_sf %>%
    dplyr::filter(ID %in% spatial_dat_sp_id)
  
  # intersect with the grids
  # and get list of grids that the species
  # occurs in
  unique_grids <- spatial_dat_sp %>%
    st_intersects(grids_5) %>%
    as.data.frame() %>%
    rename(id=col.id) %>%
    left_join(., grids_5 %>%
                st_set_geometry(NULL) %>%
                mutate(id=1:nrow(.))) %>%
    dplyr::select(grid_id) %>%
    distinct() %>%
    mutate(species=species_name)
  
}

grid_lookup <- bind_rows(lapply(species_list, species_grid_function))

# now combine this with all the grids so that it is
# zero-filled and complete
grid_lookup_full <- grids_5 %>%
  st_set_geometry(NULL) %>%
  left_join(., grid_lookup, by="grid_id")

length(unique(grid_lookup_full$grid_id))

saveRDS(grid_lookup_full, "Data/occupancy_model_data/grid_species_lookup_5.RDS")

################################
################################
full_dat_for_model <- function(species_name) {
  
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
  
  # now get all the grids that intersect with that species "range"
  grids_in_range <- ch %>%
    st_intersects(grids_5) %>%
    as.data.frame() %>%
    rename(id=col.id) %>%
    left_join(., grids_5 %>%
                st_set_geometry(NULL) %>%
                mutate(id=1:nrow(.))) %>%
    dplyr::select(grid_id) %>%
    distinct() 
  
  grids_in_range_sf <- grids_5 %>%
    dplyr::filter(grid_id %in% grids_in_range$grid_id)
  
  # grids with records
  # assign all records within the grids of the species of interest range
  # so exclude some records/grids
  unique_grids <- points_sf %>%
    st_intersects(grids_in_range_sf) %>%
    as.data.frame() %>%
    rename(id=col.id) %>%
    left_join(., grids_in_range_sf %>%
                st_set_geometry(NULL) %>%
                mutate(id=1:nrow(.))) %>%
    dplyr::select(grid_id, row.id) %>%
    rename(ID=row.id) %>%
    dplyr::select(grid_id, ID)
  
  # get data for the species summarizes
  # and prepare a dataframe for potential modelling
  dat <- unique_grids %>%
    left_join(., raw_dat, by="ID") %>%
    mutate(week=week(eventDate)) %>%
    mutate(present=ifelse(species==species_name, "yes", "no")) %>%
    group_by(grid_id, week, present) %>%
    summarize(N=n()) %>%
    mutate(present=ifelse(present=="yes", 1, 0)) %>%
    mutate(species=species_name)
  
  saveRDS(dat, paste0("Data/occupancy_model_data/species_dat_5k/", gsub(" ", "_", species_name), ".RDS"))

}

lapply(species_list, full_dat_for_model)


