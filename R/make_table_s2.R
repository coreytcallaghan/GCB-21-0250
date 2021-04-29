# an R script to make Table S2
# which includes all the data we generated plus the data
# for the traits etc.
# This script will also make an additional supplementary figure derived from this

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


dat <- readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli")

clusterResults <- readRDS("Results/clusterResults.rds")

tables2 <- left_join(clusterResults,dat,by="species")

write_csv(tables2, "Tables/table_s2.csv")
