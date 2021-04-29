# make a table of the number of observations corresponding to the countries used in analysis

library(dplyr)
library(countrycode)
library(tidyr)
library(readr)

dat <- readRDS("Data/gbif_trimmed_europe/gbif_data.RDS")

country_level_summary <- dat %>%
  group_by(countryCode) %>%
  summarize(N=n(),
            Species_Richness=length(unique(species))) %>%
  arrange(desc(N)) %>%
  mutate(Country=countrycode(countryCode, origin="iso2c", destination="country.name")) %>%
  replace_na(list(Country="Kosovo")) %>%
  dplyr::select(Country, countryCode, N, Species_Richness) %>%
  rename(`Species Richness`=Species_Richness)

# write out as csv
write_csv(country_level_summary, "Tables/table_s1.csv")
