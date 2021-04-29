# get predictor variables and response variable into one df
# for modelling and figures etc.

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

traits <- read_csv("Data/traits/coded_traits.csv")

climber <- read_delim("Data/climber/ZooKeys-367-065-s001.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# response variables
response <- readRDS("Results/butterfly_urban_scores/butterfly_urban_scores.RDS") %>%
  mutate(urban_score_med=median_lights-range_median_lights) %>%
  mutate(urban_score_25=quantile_25_lights-range_quantile_25_lights) %>%
  mutate(urban_score_75=quantile_75_lights-range_quantile_75_lights) %>%
  mutate(urban_score_mean=mean_lights-range_mean_lights) %>%
  mutate(urban_breadth=quantile_75_lights-quantile_25_lights) %>%
  mutate(urban_score_breadth=urban_score_75-urban_score_25)

matched <- traits %>%
  rename(species=Taxon) %>%
  mutate(species=gsub("_", " ", .$species)) %>%
  left_join(., response, by="species") %>%
  dplyr::filter(complete.cases(N))

not_matched <- response %>%
  dplyr::select(species) %>%
  mutate(in_data=TRUE) %>%
  dplyr::filter(! species %in% matched$species)

fixes <- not_matched %>%
  mutate(recognized_name=c(NA,
                           "Speyeria aglaja",
                           "Eumedonia eumedon",
                           "Colias crocea",
                           "Erebia albergana",
                           NA,
                           "Melitaea celadussa",
                           "Agriades optilete",
                           "Agriades orbitulus",
                           "Lysandra coridon",
                           NA))

analysis_dat <- fixes %>%
  left_join(., response, by="species") %>%
  dplyr::select(-species, -in_data) %>%
  rename(species=recognized_name) %>%
  left_join(., traits %>%
              rename(species=Taxon) %>%
              mutate(species=gsub("_", " ", .$species))) %>%
  dplyr::filter(complete.cases(Family)) %>%
  bind_rows(matched) %>%
  dplyr::select(species, Family, N, range_N, urban_score_mean,
                urban_score_25, urban_score_75, 
                OvS, FMo_Average, WIn, Vol_min, Vol_max, TrC,
                HSI, ELT, ELL, ADF, HPG) %>%
  mutate(voltinism_mean=(Vol_min+Vol_max)/2) %>%
  dplyr::select(-Vol_min, -Vol_max) %>%
  rename(overwintering_stage=OvS,
         flight_months_average=FMo_Average,
         wing_index=WIn,
         hostplant_specificity=TrC,
         hostplant_index=HSI,
         egg_laying_type=ELT,
         egg_laying_location=ELL,
         adult_food_types=ADF,
         hostplant_growth_form=HPG) %>%
  left_join(., climber %>%
              dplyr::select(1, 5) %>%
              rename(species=1))

climber_matched <- analysis_dat %>%
  dplyr::filter(complete.cases(temp.mean))

not_matched_climber <- analysis_dat %>%
  dplyr::filter(!species %in% climber_matched$species) %>%
  dplyr::select(species)

fixes <- data.frame(species=not_matched_climber$species,
                    species_climber=c("Argynnis aglaja",
                                      "Aricia eumedon",
                                      "Erebia alberganus",
                                      NA,
                                      "Plebejus optilete",
                                      "Plebejus orbitulus",
                                      "Polyommatus coridon",
                                      NA,
                                      "Carterocephalus silvicolus",
                                      "Syrichtus proto",
                                      "Pyrgus malvae",
                                      NA,
                                      NA,
                                      NA,
                                      "Scolitantides panoptes",
                                      "Scolitantides baton",
                                      "Polyommatus semiargus",
                                      "Argynnis niobe",
                                      "Argynnis adippe",
                                      "Nymphalis egea",
                                      "Nymphalis c-album"))

temp <- analysis_dat %>%
  dplyr::filter(species %in% fixes$species) %>%
  left_join(., fixes) %>%
  dplyr::select(-temp.mean) %>%
  left_join(., climber %>%
              dplyr::select(1, 5) %>%
              rename(species_climber=1)) %>%
  dplyr::select(-species_climber)

analysis_dat <- climber_matched %>%
  bind_rows(temp) %>% 
  # do a bit more manipulations to calculate response variables
  mutate(egg_laying_location=nchar(egg_laying_location)/2) %>%
  mutate(hostplant_growth_Sh=ifelse(str_detect(.$hostplant_growth_form, "Sh")==TRUE, 1, 0)) %>%
  mutate(hostplant_growth_Th=ifelse(str_detect(.$hostplant_growth_form, "Th")==TRUE, 1, 0)) %>%
  mutate(hostplant_growth_Sb=ifelse(str_detect(.$hostplant_growth_form, "Sb")==TRUE, 1, 0)) %>%
  mutate(hostplant_growth_Tr=ifelse(str_detect(.$hostplant_growth_form, "Tr")==TRUE, 1, 0)) %>%
  mutate(hostplant_growth_form=nchar(hostplant_growth_form)/2) %>%
  mutate(adult_food_types=nchar(adult_food_types)/2) %>%
  mutate(egg_laying_type=ifelse(nchar(egg_laying_type)==2, egg_laying_type, str_sub(egg_laying_type, start= -2))) %>%
  rename(temp_mean=temp.mean) %>%
  mutate(overwintering_stage_E=ifelse(str_detect(.$overwintering_stage, "E")==TRUE, 1, 0)) %>%
  mutate(overwintering_stage_L=ifelse(str_detect(.$overwintering_stage, "L")==TRUE, 1, 0)) %>%
  mutate(overwintering_stage_P=ifelse(str_detect(.$overwintering_stage, "P")==TRUE, 1, 0)) %>%
  mutate(overwintering_stage_A=ifelse(str_detect(.$overwintering_stage, "A")==TRUE, 1, 0)) %>%
  mutate(overwintering_stage_ordinal=case_when(overwintering_stage=="E" ~ 1,
                                       overwintering_stage=="EL" ~ 1.5,
                                       overwintering_stage=="L" ~ 2,
                                       overwintering_stage=="ELP" ~ 2.5,
                                       overwintering_stage=="LP" ~ 3,
                                       overwintering_stage=="P" ~ 3.5,
                                       overwintering_stage=="ELPA" ~ 4,
                                       overwintering_stage=="A" ~ 4.5))
  

saveRDS(analysis_dat, "Results/data_for_analysis.RDS")











