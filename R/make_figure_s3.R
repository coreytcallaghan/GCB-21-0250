# make figure s2
# example of ggridges for some species
# for supplementary figure

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)

# read in the different datasets
viirs <- readRDS("Data/night_lights_data/gbif_viirs_lights_export.RDS")

raw_dat <- readRDS("Data/gbif_trimmed_europe/gbif_data.RDS")


# 15 random species
species_list <-  readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli") %>%
  dplyr::select(species) %>%
  sample_n(15) %>%
  .$species

ex_dat <- raw_dat %>%
  left_join(., viirs) %>%
  ungroup() %>%
  dplyr::filter(species %in% species_list) %>%
  dplyr::filter(viirs < quantile(viirs, 0.95))

ggplot(ex_dat, aes(x=viirs, y=species, group=species, fill=stat(x)))+
  geom_density_ridges_gradient(scale=1)+
  scale_fill_viridis_c()+
  guides(fill=FALSE)+
  scale_x_log10()+
  ylab("")+
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_text(face="italic"))

ggsave("Figures/example_ggridges.png", height=5.5, width=7, units="in")
