# An R script to summarize the response variables

# packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

dat <- readRDS("Results/butterfly_urban_scores/butterfly_urban_scores.RDS")

# make a figure showing a histogram of the median, mean, 25, and 75 quantiles
dat %>%
  dplyr::select(1, 3:6) %>%
  pivot_longer(!species, names_to="variable", values_to="value") %>%
  ggplot(., aes(x=value))+
  geom_histogram(color="black", fill="gray80")+
  theme_bw()+
  facet_wrap(~variable, scales="free")+
  scale_x_log10()+
  ylab("Number of species")+
  xlab("Distribution summary (log10)")

# make another figure showing the different species
dat %>%
  sample_n(30) %>%
  arrange(median_lights) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=median_lights), color="blue")+
  geom_point(aes(x=species, y=quantile_75_lights), color="red")+
  geom_point(aes(x=species, y=quantile_25_lights), color="red")+
  coord_flip()+
  scale_y_log10()+
  theme_bw()+
  xlab("Species")+
  ylab("Distribution summary (log10)")

# make the same as above, but add the range adjustment
dat2 <- dat %>%
  mutate(urban_score_med=median_lights-range_median_lights) %>%
  mutate(urban_score_25=quantile_25_lights-range_quantile_25_lights) %>%
  mutate(urban_score_75=quantile_75_lights-range_quantile_75_lights) %>%
  mutate(urban_score_mean=mean_lights-range_mean_lights) %>%
  mutate(urban_breadth=quantile_75_lights-quantile_25_lights) %>%
  mutate(urban_score_breadth=urban_score_75-urban_score_25)

dat2 %>%
  dplyr::select(1, urban_score_med, urban_score_25, urban_score_75, urban_score_mean) %>%
  pivot_longer(!species, names_to="variable", values_to="value") %>%
  ggplot(., aes(x=value))+
  geom_histogram(color="black", fill="gray80")+
  theme_bw()+
  facet_wrap(~variable, scales="free")+
  ylab("Number of species")+
  xlab("Distribution summary (log10)")

dat2 %>% 
  dplyr::filter(urban_score_mean<8) %>% 
  ggplot(., aes(x=urban_breadth, y=abs(urban_score_mean)))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urban breadth")+
  ylab("abs(Urban tolerance score)")

dat2 %>% 
  dplyr::filter(urban_score_mean<8) %>% 
  ggplot(., aes(x=urban_breadth, y=urban_score_mean))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urban breadth")+
  ylab("Urban tolerance score")

dat2 %>% 
  dplyr::filter(urban_score_mean<8) %>% 
  ggplot(., aes(x=urban_score_breadth, y=urban_score_mean))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urban breadth")+
  ylab("Urban tolerance score")+
  geom_smooth(method="lm", color="#D95F02")+
  ggtitle(paste0("N=", nrow(dat2 %>% 
                              dplyr::filter(urban_score_mean<8)), " species"))

ggsave("Figures/urban_breadth_vs_urban_tolerance_score.png", width=6, height=5, units="in")

dat2 %>%
  sample_n(30) %>%
  arrange(urban_score_med) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=urban_score_med), color="blue")+
  #geom_point(aes(x=species, y=urban_score_25), color="red")+
  #geom_point(aes(x=species, y=urban_score_75), color="red")+
  coord_flip()+
  theme_bw()+
  xlab("Species")+
  ylab("Distribution summary (log10)")

dat2 %>%
  ggplot(., aes(x=urban_score_med, y=median_lights))+
  geom_point()+
  theme_bw()

# but see what happens when we remove these couple of 'outliers'
dat3 <- dat2 %>%
  dplyr::filter(!species %in% c("Zizeeria knysna", "Cacyreus marshalli", "Danaus plexippus", "Polygonia egea"))

# so now try plotting these figures again
dat3 %>%
  dplyr::select(1, urban_score_med, urban_score_25, urban_score_75, urban_score_mean) %>%
  pivot_longer(!species, names_to="variable", values_to="value") %>%
  ggplot(., aes(x=value))+
  geom_histogram(color="black", fill="gray80")+
  theme_bw()+
  facet_wrap(~variable, scales="free")+
  ylab("Number of species")+
  xlab("Distribution summary (log10)")



dat3 %>%
  sample_n(60) %>%
  arrange(urban_score_med) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=urban_score_med), color="blue")+
  #geom_point(aes(x=species, y=urban_score_25), color="red")+
  #geom_point(aes(x=species, y=urban_score_75), color="red")+
  coord_flip()+
  theme_bw()+
  xlab("Species")+
  ylab("Distribution summary (log10)")+
  geom_hline(yintercept=0, linetype="dashed", color="red")+
  theme(axis.text.y=element_text(size=7))

# make a figure showing a histogram of the median, mean, 25, and 75 quantiles
dat3 %>%
  dplyr::select(1, 3:6) %>%
  pivot_longer(!species, names_to="variable", values_to="value") %>%
  ggplot(., aes(x=value))+
  geom_histogram(color="black", fill="gray80")+
  theme_bw()+
  facet_wrap(~variable, scales="free")+
  scale_x_log10()+
  ylab("Number of species")+
  xlab("Distribution summary (log10)")

# make another figure showing the different species
dat3 %>%
  sample_n(30) %>%
  arrange(median_lights) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=median_lights), color="blue")+
  geom_point(aes(x=species, y=quantile_75_lights), color="red")+
  geom_point(aes(x=species, y=quantile_25_lights), color="red")+
  coord_flip()+
  scale_y_log10()+
  theme_bw()+
  xlab("Species")+
  ylab("Distribution summary (log10)")
