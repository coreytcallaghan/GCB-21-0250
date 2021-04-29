# investigate the relationship between unmarked modelling
# and the median/mean approach of the distribution

# packages
library(dplyr)
library(ggplot2)
library(patchwork)

urban_scores <- readRDS("Results/butterfly_urban_scores/butterfly_urban_scores.RDS")

occ_results <- readRDS("Results/occ_models/output_occmodels.rds")

species_list <-  readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli") %>%
  dplyr::select(species) %>%
  .$species

dat <- occ_results %>%
  rename(species=Species) %>%
  left_join(., urban_scores, by="species") %>%
  dplyr::filter(species %in% species_list) %>%
  mutate(urban_score_med=median_lights-range_median_lights) %>%
  mutate(urban_score_25=quantile_25_lights-range_quantile_25_lights) %>%
  mutate(urban_score_75=quantile_75_lights-range_quantile_75_lights) %>%
  mutate(urban_score_mean=mean_lights-range_mean_lights) %>%
  dplyr::filter(urbanResponse_SE<2)

# quick plot of raw median vs unmarked response
ggplot(dat, aes(y=urbanResponse, x=median_lights))+
  geom_point()+
  theme_bw()+
  scale_x_log10()+
  geom_smooth(method="lm")

ggplot(dat, aes(y=urbanResponse, x=mean_lights))+
  geom_point()+
  theme_bw()+
  scale_x_log10()+
  geom_smooth(method="lm")

dat %>%
  dplyr::filter(urban_score_med <5) %>%
  ggplot(., aes(y=urbanResponse, x=urban_score_med))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")

a <- dat %>%
  #dplyr::filter(urban_score_mean <10) %>%
  ggplot(., aes(y=urbanResponse, x=urban_score_mean))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm", color="#D95F02")+
  theme_bw()+
  xlab("Urban affinity score (distribution)")+
  ylab("Urban affinity score (unmarked model)")+
  theme(axis.text=element_text(color="black"))+
  ggtitle(paste0("N=", nrow(dat), " species"))

a


ggsave("Figures/unmarked_vs_distribution_approach.png", height=5.5, width=7, units="in")
