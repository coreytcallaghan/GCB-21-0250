# this script pulls together the main analysis
# first an overview/summary of the data with some summary figures
# then it looks at modelling the relationships
# between urban scores and traits to see if there are any patterns


library(dplyr)
library(ggplot2)
library(tidyr)
library(ggcorrplot)
library(forcats)
library(GGally)
library(ggcorrplot)
library(olsrr)
library(MASS)
library(dismo)
library(gbm)
library(tibble)
library(patchwork)

dat <- readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli")

# are urban score mean
# and the quantiles positively related
dat %>%
  dplyr::select(urban_score_25, urban_score_mean, urban_score_75) %>%
  cor(.) %>%
  ggcorrplot(hc.order = TRUE, lab = TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("")

# save out figure, but I don't think it is worth including it in the manuscript per se
#ggsave("Figures/correlation_between_.25_.75_and_mean.png", width=4.5, height=4.5, units="in")
dat %>%
  mutate(urban_score_breadth=urban_score_75-urban_score_25) %>%
  ggplot(., aes(x=urban_score_breadth, y=urban_score_mean))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urban breadth")+
  ylab("Urban affinity score")+
  geom_smooth(method="lm", color="#D95F02")

#ggsave("Figures/urban_breadth_vs_urban_tolerance_score.png", height=5.5, width=7, units="in")

# strong correlation between the three possible responses
# I think this is support to just stick with the mean
# as the measure of urban tolerance/preference/affinity for each species

# plot a histogram of the 158 species included in the analysis
# this should be Figure 1a potentially
ggplot(dat, aes(x=urban_score_mean))+
  geom_histogram(color="black", fill="gray80", bins=50)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_vline(xintercept=0, linetype="dashed", color="#D95F02", size=1.4)+
  xlab("Urban affinity score")+
  ylab("Number of species")

#ggsave("Figures/histogram_of_urban_scores.png", width=5.8, height=5.2, units="in")

# now let's show the species 'ranked' where each point represents a species
dat %>%
  arrange(urban_score_mean) %>%
  mutate(response=ifelse(urban_score_mean>0, "positive", "negative")) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=urban_score_mean, color=response), size=0.7)+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Urban affinity score")+
  geom_hline(yintercept=0, linetype="dashed", color="#D95F02", size=0.8)+
  theme(axis.text.y=element_text(size=5.3, face="italic"))+
  scale_color_brewer(palette="Dark2", direction=-1)+
  guides(color=FALSE)

#ggsave("Figures/butterfly_ranks.png", height=8.8, width=5.8, units="in")

# make an interactive version of this figure
# make an ineractive figure with the residuals of each town 'ranking' them
library(plotly)
library(htmlwidgets)

fig <- dat %>%
  arrange(urban_score_mean) %>%
  mutate(response=ifelse(urban_score_mean>0, "positive", "negative")) %>%
  mutate(Species=species) %>%
  rename(`Urban tolerance score`=urban_score_mean) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=`Urban tolerance score`, color=response, label=Species), size=0.7)+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Urban affinity score")+
  geom_hline(yintercept=0, linetype="dashed", color="gray80", size=0.8)+
  theme(axis.text.y=element_text(size=5.3, face="italic"))+
  scale_color_brewer(palette="Dark2", direction=-1)+
  guides(color=FALSE)+
  ylim(-5.0, 5.0)

plot_bits <- ggplot_build(fig)$urban_score_mean

fig_p <- ggplotly(fig, width = 900, height = 850, tooltip=c("y", "label"))

fig_p2 <- hide_legend(fig_p)

#setwd("interactive_butterfly_ranks")
#saveWidget(widget = fig_p2, file = "butterfly_ranks.html")
#setwd("..")

# but because we have 158 species
# I think it is tough to show all these species in one figure for the main paper
# instead, I'll randomly choose 60 species and make a figure of them
# now let's show the species 'ranked' where each point represents a species
dat %>%
  sample_n(60) %>%
  arrange(urban_score_mean) %>%
  mutate(response=ifelse(urban_score_mean>0, "positive", "negative")) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(species), y=urban_score_mean, color=response), size=1.1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Urban affinity score")+
  geom_hline(yintercept=0, linetype="dashed", color="#D95F02", size=0.8)+
  theme(axis.text.y=element_text(size=9.3, face="italic"))+
  scale_color_brewer(palette="Set2", direction=-1)+
  guides(color=FALSE)

#ggsave("Figures/butterfly_ranks.png", height=8.8, width=5.8, units="in")

# some summary stuff of empirical results for the paper
# total number of observations used
sum(dat$N)

# total number of species
nrow(dat)

mean(dat$N)
sd(dat$N)

# percent of species with score < 0
dat %>%
  dplyr::filter(urban_score_mean<0) %>%
  nrow(.)/nrow(dat)*100


mean(dat$urban_score_mean)
sd(dat$urban_score_mean)


# now start exploring some of the predictor variables
# start with a ggpairs plot
dat %>%
  dplyr::select(9:19) %>%
  ggpairs(.)


# looks like adult food types
# and hostplant index
# could be log-transformed?
# both other than that, everything looks kinda messy, albeit not too bad.
# some relatively strong correlations between the hostplant measurements etc.
# but nothing to get too worried about at this point
hist(dat$adult_food_types)
hist(log10(dat$adult_food_types))
hist(dat$hostplant_index)
hist(log10(dat$hostplant_index))

# I'll add the log10 versions of each of adult food types
# and of hostplant index

dat <- dat %>%
  mutate(adult_food_types_log10=log10(adult_food_types)) %>%
  mutate(hostplant_index_log10=log10(hostplant_index))

#################################################
#################################################
####### OPTION 1: pairwise comparisons ##########
#################################################
#################################################
# now let's use a corrplot object
# including the response variable of urban tolerance
# this will allow us to look at initial patterns of correlation
dat %>%
  dplyr::select(species, urban_score_mean, 9:18, 27, 28, 29) %>%
  dplyr::select(-adult_food_types, -hostplant_index) %>%
  dplyr::select(-egg_laying_type) %>%
  pivot_longer(cols=2:12, names_to="var", values_to="value") %>%
  mutate(trait_clean=case_when(var=="flight_months_average" ~ "Average number of flight months",
                               var=="wing_index" ~ "Wing index",
                               var=="temp_mean" ~ "Mean temperature in range",
                               var=="adult_food_types_log10" ~ "Number of adult food types (log10)",
                               var=="hostplant_index_log10" ~ "Hostplant specialism index (log10)",
                               var=="voltinism_mean" ~ "Mean voltinism",
                               var=="hostplant_growth_form" ~ "Number of hostplant growth forms",
                               var=="egg_laying_location" ~ "Number of egg laying locations",
                               var=="overwintering_stage_ordinal" ~ "Overwintering stage (ordinal)",
                               var=="hostplant_specificity" ~ "Hostplant specificity",
                               var=="urban_score_mean" ~ "Urban affinity score")) %>%
  dplyr::select(-var) %>%
  pivot_wider(names_from="trait_clean", values_from=value) %>%
  column_to_rownames(var="species") %>%
  cor(.) %>%
  ggcorrplot(lab=TRUE,
             method="circle",
             colors = c("#6D9EC1", "white", "#E46726"),
             lab_size = 1.6)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1, color=c("red", "black", "black", "black", "black",
                                                            "black", "black", "black", "black", "black", "black"), size=5.5))+
  theme(axis.text.y=element_text(color=c("red", "black", "black", "black", "black", "black", 
                                        "black", "black", "black", "black", "black"), size=5.5))+
  theme(axis.ticks=element_blank())+
  theme(legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.height = unit(0.4, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), #change legend key width
        legend.title = element_text(size=6), #change legend title font size
        legend.text = element_text(size=5.5))+
  scale_x_discrete(labels=c("Urban affinity score", "Average number of flight months", "Mean voltinism", "Number of hostplant growth forms",
                            "Overwintering stage (ordinal)", "Number of adult food types (log10)", "Hostplant specialism index (log10)",
                            "Hostplant specificity", "Wing index", "Mean temperature in range", "Number of egg laying locations"),
                   breaks=c("Urban affinity score", "Average number of flight months", "Mean voltinism", "Number of hostplant growth forms",
                            "Overwintering stage (ordinal)", "Number of adult food types (log10)", "Hostplant specialism index (log10)",
                            "Hostplant specificity", "Wing index", "Mean temperature in range", "Number of egg laying locations"),
                   limits=c("Urban affinity score", "Average number of flight months", "Mean voltinism", "Number of hostplant growth forms",
                            "Overwintering stage (ordinal)", "Number of adult food types (log10)", "Hostplant specialism index (log10)",
                            "Hostplant specificity", "Wing index", "Mean temperature in range", "Number of egg laying locations"))+
  scale_y_discrete(labels=c("Urban affinity score", "Average number of flight months", "Mean voltinism", "Number of hostplant growth forms",
                            "Overwintering stage (ordinal)", "Number of adult food types (log10)", "Hostplant specialism index (log10)",
                            "Hostplant specificity", "Wing index", "Mean temperature in range", "Number of egg laying locations"),
                   breaks=c("Urban affinity score", "Average number of flight months", "Mean voltinism", "Number of hostplant growth forms",
                            "Overwintering stage (ordinal)", "Number of adult food types (log10)", "Hostplant specialism index (log10)",
                            "Hostplant specificity", "Wing index", "Mean temperature in range", "Number of egg laying locations"),
                   limits=c("Urban affinity score", "Average number of flight months", "Mean voltinism", "Number of hostplant growth forms",
                            "Overwintering stage (ordinal)", "Number of adult food types (log10)", "Hostplant specialism index (log10)",
                            "Hostplant specificity", "Wing index", "Mean temperature in range", "Number of egg laying locations"))+
  scale_size(range = c(1, 6))
                            

#ggsave("Figures/pairwise_relationships_numeric.png", width=4.2, height=4.3, units="in")

#######################################################
#######################################################
####### OPTION 2: multiple linear regression ##########
#######################################################
#######################################################
# start running some linear models to see what best predicts
# urban tolerance in butterflies
model_data <- dat %>%
  dplyr::select(N, urban_score_mean, 9:29) %>%
  dplyr::select(-adult_food_types, -hostplant_index) %>%
  mutate(weights=ifelse(N<1000, N, 1000)) %>%
  mutate(egg_laying_type_dummy=case_when(egg_laying_type=="Se" ~ 1,
                                   egg_laying_type=="Lb" ~ 2,
                                   egg_laying_type=="Sb" ~ 3)) %>%
  as.data.frame()

# quick model to see if there is any support for the difference between egg laying types
egg_type <- lm(urban_score_mean ~ egg_laying_type, data=model_data, weights=weights)
summary(egg_type)

# no support, so let's convert this to a dummy coded variable for
# the multiple linear regression
model_data <- model_data %>%
  dplyr::select(-egg_laying_type) %>%
  rename(egg_laying_type=egg_laying_type_dummy) %>%
  dplyr::select(1:6, 22, 7:21)

# check if there are any explanation
# between binomial categories of hostplant growth
mod_hostplant_growth <- lm(urban_score_mean ~ hostplant_growth_Sh + hostplant_growth_Th +
              hostplant_growth_Sb + hostplant_growth_Tr, 
            data=model_data, weights=weights)
summary(mod_hostplant_growth)

# get data out of model for plotting results
estimates_hostplant_mod <- broom::tidy(mod_hostplant_growth) %>%
  mutate(upper_95_cl=confint(mod_hostplant_growth)[,2]) %>%
  mutate(lower_95_cl=confint(mod_hostplant_growth)[,1]) %>%
  mutate(trait_clean=case_when(term=="hostplant_growth_Sb" ~ "Hostplant-shrub (binomial)",
                               term=="hostplant_growth_Th" ~ "Hostplant-tall herb/grass (binomial)",
                               term=="hostplant_growth_Tr" ~ "Hostplant-tree (binomial)",
                               term=="hostplant_growth_Sh" ~ "Hostplant-short herb/grass (binomial)",
                               term=="(Intercept)" ~ "(Intercept)"))

ggplot(estimates_hostplant_mod, aes(x=trait_clean, y=estimate))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_errorbar(aes(ymin=lower_95_cl, ymax=upper_95_cl))+
  xlab("")+
  ylab("Parameter estimate")+
  geom_hline(yintercept=0, linetype="dashed", color="#D95F02")+
  scale_x_discrete(limits=c("Hostplant-shrub (binomial)", "Hostplant-tall herb/grass (binomial)", "Hostplant-tree (binomial)", "Hostplant-short herb/grass (binomial)"),
                   labels=c("Hostplant-shrub (binomial)", "Hostplant-tall herb/grass (binomial)", "Hostplant-tree (binomial)", "Hostplant-short herb/grass (binomial)"))

#ggsave("Figures/hostplant_separate_model_results.png", height=5.7, width=6.4, units="in")

# and then check for binomial categories of overwintering stage
mod_overwintering_stage <- lm(urban_score_mean ~ overwintering_stage_E + overwintering_stage_L +
                                overwintering_stage_P + overwintering_stage_A, 
                           data=model_data, weights=weights)
summary(mod_overwintering_stage)

# get data out of model for plotting results
estimates_overwintering_mod <- broom::tidy(mod_overwintering_stage) %>%
  mutate(upper_95_cl=confint(mod_overwintering_stage)[,2]) %>%
  mutate(lower_95_cl=confint(mod_overwintering_stage)[,1]) %>%
  mutate(trait_clean=case_when(term=="overwintering_stage_L" ~ "Larval-overwintering (binomial)",
                               term=="overwintering_stage_P" ~ "Pupal-overwintering (binomial)",
                               term=="overwintering_stage_E" ~ "Egg-overwintering (binomial)",
                               term=="overwintering_stage_A" ~ "Adult-overwintering (binomial)",
                               term=="(Intercept)" ~ "(Intercept)"))

ggplot(estimates_overwintering_mod, aes(x=trait_clean, y=estimate))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_errorbar(aes(ymin=lower_95_cl, ymax=upper_95_cl))+
  xlab("")+
  ylab("Parameter estimate")+
  geom_hline(yintercept=0, linetype="dashed", color="#D95F02")+
  scale_x_discrete(limits=c("Egg-overwintering (binomial)", "Larval-overwintering (binomial)", "Pupal-overwintering (binomial)", "Adult-overwintering (binomial)"),
                   labels=c("Egg-overwintering (binomial)", "Larval-overwintering (binomial)", "Pupal-overwintering (binomial)", "Adult-overwintering (binomial)"))

#ggsave("Figures/overwintering_separate_model_results.png", height=5.7, width=6.4, units="in")

# run a giant model
# with all the traits and predictions (beside the binomial ones)
big_mod <- lm(urban_score_mean ~ flight_months_average + wing_index +
                hostplant_specificity + egg_laying_type + egg_laying_location +
                hostplant_growth_form + voltinism_mean + temp_mean + 
                adult_food_types_log10 + hostplant_index_log10 + overwintering_stage_ordinal,
              data=model_data, weights=weights)
summary(big_mod)

# standardize coefficients
scaled_big_mod <- arm::standardize(big_mod)

summary(scaled_big_mod)

# get data out of model for plotting results
estimates_big_mod <- broom::tidy(scaled_big_mod) %>%
  mutate(upper_95_cl=confint(scaled_big_mod)[,2]) %>%
  mutate(lower_95_cl=confint(scaled_big_mod)[,1]) %>%
  mutate(var=gsub("z.", "", term)) %>%
  mutate(trait_clean=case_when(var=="flight_months_average" ~ "Average number of flight months",
                               var=="wing_index" ~ "Wing index",
                               var=="temp_mean" ~ "Mean temperature in range",
                               var=="adult_food_types_log10" ~ "Number of adult food types (log10)",
                               var=="hostplant_index_log10" ~ "Hostplant specialism index (log10)",
                               var=="overwintering_stage_L" ~ "Larval-overwintering (binomial)",
                               var=="voltinism_mean" ~ "Mean voltinism",
                               var=="hostplant_growth_form" ~ "Number of hostplant growth forms",
                               var=="hostplant_growth_Sb" ~ "Hostplant-shrub (binomial)",
                               var=="egg_laying_location" ~ "Number of egg laying locations",
                               var=="egg_laying_type" ~ "Egg laying type",
                               var=="overwintering_stage_ordinal" ~ "Overwintering stage (ordinal)",
                               var=="hostplant_growth_Th" ~ "Hostplant-tall herb/grass (binomial)",
                               var=="hostplant_specificity" ~ "Hostplant specificity",
                               var=="hostplant_growth_Tr" ~ "Hostplant-tree (binomial)",
                               var=="hostplant_growth_Sh" ~ "Hostplant-short herb/grass (binomial)",
                               var=="overwintering_stage_P" ~ "Pupal-overwintering (binomial)",
                               var=="overwintering_stage_E" ~ "Egg-overwintering (binomial)",
                               var=="overwintering_stage_A" ~ "Adult-overwintering (binomial)")) %>%
  dplyr::filter(complete.cases(trait_clean)) %>%
  arrange(estimate)

ggplot(estimates_big_mod, aes(x=fct_inorder(trait_clean), y=estimate))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=5.5))+
  geom_errorbar(aes(ymin=lower_95_cl, ymax=upper_95_cl))+
  xlab("")+
  ylab("Standardized parameter estimate")+
  geom_hline(yintercept=0, linetype="dashed", color="#D95F02")+
  theme(axis.title=element_text(size=6))

#ggsave("Figures/multiple_linear_regression_results.png", width=4.7, height=3.8, units="in")

#################################
#################################
####### OPTION 3: BRTs ##########
#################################
#################################
set.seed(123)

brt.tc5.lr01 <- gbm.step(data=model_data, 
                         gbm.x=3:21,
                         gbm.y=2,
                         family="gaussian",
                         tree.complexity=5,
                         learning.rate=0.01,
                         bag.fraction=0.5)

brt.tc5.lr01$cv.statistics
brt.tc5.lr01$weights
brt.tc5.lr01$n.trees

summary(brt.tc5.lr01)

# But need to test a few different levels of complexity and
# learning rate to see how the model performs
# could functionalize this and plot results
brt.tc5.lr001 <- gbm.step(data=model_data, 
                         gbm.x=3:21,
                         gbm.y=2,
                         family="gaussian",
                         tree.complexity=5,
                         learning.rate=0.001,
                         bag.fraction=0.5)

brt.tc5.lr001$cv.statistics
brt.tc5.lr001$weights
brt.tc5.lr001$n.trees

summary(brt.tc5.lr001)

brt.tc7.lr001 <- gbm.step(data=model_data, 
                          gbm.x=3:21,
                          gbm.y=2,
                          family="gaussian",
                          tree.complexity=7,
                          learning.rate=0.001,
                          bag.fraction=0.5)

brt.tc7.lr001$cv.statistics
brt.tc7.lr001$weights
brt.tc7.lr001$n.trees

summary(brt.tc7.lr001)

brt.tc3.lr001 <- gbm.step(data=model_data, 
                          gbm.x=3:21,
                          gbm.y=2,
                          family="gaussian",
                          tree.complexity=3,
                          learning.rate=0.001,
                          bag.fraction=0.5)

brt.tc3.lr001$cv.statistics
brt.tc3.lr001$weights
brt.tc3.lr001$n.trees

summary(brt.tc3.lr001)


# these are all strikingly similar in both their
# cv statistics and the summary results - the same variables
# come out as important in each different model
# so lets stick with tree complexity of 5, lr 0.001, bag.fraction=0.5
brt_mod <- gbm.step(data=model_data, 
                    gbm.x=3:21,
                    gbm.y=2,
                    family="gaussian",
                    tree.complexity=5,
                    learning.rate=0.001,
                    bag.fraction=0.5)

brt_mod$cv.statistics
brt_mod$weights
brt_mod$n.trees

summary(brt_mod)

# manual calculate total deviance explained
(2.553-1.945)/2.553
# Now need to plot the results from the brt
gbm.plot(brt_mod, n.plots=10, write.title=TRUE)


# take a quick look for any interactions
find.int <- gbm.interactions(brt_mod)
find.int$interactions
find.int$rank.list

# looks like a somewhat strong interaction between flight months average and temp mean
# so lets just plot this as a test
# not sure whether we want to go down this road, but good to have in our back pocket
# in case reviewers ask etc.
gbm.perspec(brt_mod, 8, 1, y.range=c(8,20), z.range=c(0,0.6))

gbm.perspec(brt_mod, 14, 8)

gbm.perspec(brt_mod, 8, 19)

gbm.perspec(brt_mod, 18, 19)

# now let's try to make a pretty (ggplot2) version 
# of the boosted regression tree results
# will plot any variable that has > 5% contribution to the model
relative_influence <- summary(brt_mod)

n <- relative_influence %>%
  dplyr::filter(rel.inf>=5) %>%
  nrow(.)

# This is super hacky!! But I think it works
# as I checked each variable manually
data_for_plots <- gbm::plot.gbm(brt_mod, 1, return.grid = TRUE) %>%
  as.data.frame() %>%
  mutate(trait="flight_months_average") %>%
  rename(x=1) %>%
  bind_rows(gbm::plot.gbm(brt_mod, 2, return.grid = TRUE) %>%
              as.data.frame() %>%
              mutate(trait="wing_index") %>%
              rename(x=1)) %>%
  bind_rows(gbm::plot.gbm(brt_mod, 8, return.grid = TRUE) %>%
              as.data.frame() %>%
              mutate(trait="temp_mean") %>%
              rename(x=1)) %>%
  bind_rows(gbm::plot.gbm(brt_mod, 18, return.grid = TRUE) %>%
              as.data.frame() %>%
              mutate(trait="adult_food_types_log10") %>%
              rename(x=1)) %>%
  bind_rows(gbm::plot.gbm(brt_mod, 19, return.grid = TRUE) %>%
              as.data.frame() %>%
              mutate(trait="hostplant_index_log10") %>%
              rename(x=1)) %>%
  bind_rows(gbm::plot.gbm(brt_mod, 14, return.grid = TRUE) %>%
              as.data.frame() %>%
              mutate(trait="overwintering_stage_L") %>%
              rename(x=1))

# The numbers/% variable importance/expalined are
# manually added, so this might kick an error
# and it is just a matter of updating these values and the plot should return fine
# (They change slightly due to the randomness of the brts)
data_for_plots %>%
  left_join(., relative_influence %>%
              rename(trait=var), by="trait") %>%
  mutate(rel.inf=round(rel.inf, digits=1)) %>%
  mutate(relative_influence=as.character(paste0(rel.inf, "%"))) %>%
  mutate(trait_clean=case_when(trait=="flight_months_average" ~ "Average number of flight months",
                               trait=="wing_index" ~ "Wing index",
                               trait=="temp_mean" ~ "Mean temperature in range",
                               trait=="adult_food_types_log10" ~ "Number of adult food types (log10)",
                               trait=="hostplant_index_log10" ~ "Hostplant specialism index (log10)",
                               trait=="overwintering_stage_L" ~ "Larval-overwintering (binomial)")) %>%
  mutate(label=factor(paste0(trait_clean, " (", relative_influence, ")"),
                      levels=c("Average number of flight months (34.7%)",
                               "Mean temperature in range (14.7%)",
                               "Hostplant specialism index (log10) (11.2%)",
                               "Wing index (11.1%)",
                               "Larval-overwintering (binomial) (7.4%)",
                               "Number of adult food types (log10) (6%)"))) %>%
  ggplot(., aes(x=x, y=y))+
  geom_line()+
  facet_wrap(~label, scales="free_x")+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=5.5))+
  ylab("Fitted function")+
  xlab("Scaled predictor variable")+
  theme(strip.text.x = element_text(size = 4.3))+
  theme(axis.title=element_text(size=6))

#ggsave("Figures/brt_partial_dependency_plots.png", width=4.7, height=3.8, units="in")

gbm.plot(brt_mod, n.plots=6, write.title=TRUE)

# Looks good, even if it take a bit of ugly work to patch together!
# This is only for those traits >5% relative contribution

# I think also make a simple bar chart for the relative contribution results of
# all the predictors
# or the overall summary of the brt model
relative_influence %>%
  mutate(trait_clean=case_when(var=="flight_months_average" ~ "Average number of flight months",
                               var=="wing_index" ~ "Wing index",
                               var=="temp_mean" ~ "Mean temperature in range",
                               var=="adult_food_types_log10" ~ "Number of adult food types (log10)",
                               var=="hostplant_index_log10" ~ "Hostplant specialism index (log10)",
                               var=="overwintering_stage_L" ~ "Larval-overwintering (binomial)",
                               var=="voltinism_mean" ~ "Mean voltinism",
                               var=="hostplant_growth_form" ~ "Number of hostplant growth forms",
                               var=="hostplant_growth_Sb" ~ "Hostplant-shrub (binomial)",
                               var=="egg_laying_location" ~ "Number of egg laying locations",
                               var=="egg_laying_type" ~ "Egg laying type",
                               var=="overwintering_stage_ordinal" ~ "Overwintering stage (ordinal)",
                               var=="hostplant_growth_Th" ~ "Hostplant-tall herb/grass (binomial)",
                               var=="hostplant_specificity" ~ "Hostplant specificity",
                               var=="hostplant_growth_Tr" ~ "Hostplant-tree (binomial)",
                               var=="hostplant_growth_Sh" ~ "Hostplant-short herb/grass (binomial)",
                               var=="overwintering_stage_P" ~ "Pupal-overwintering (binomial)",
                               var=="overwintering_stage_E" ~ "Egg-overwintering (binomial)",
                               var=="overwintering_stage_A" ~ "Adult-overwintering (binomial)")) %>%
  arrange(rel.inf) %>%
  ggplot(., aes(x=fct_inorder(trait_clean), y=rel.inf))+
  geom_col()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=5.5))+
  xlab("")+
  ylab("Relative influence")+
  theme(axis.title.y=element_text(size=6))

#ggsave("Figures/brt_relative_influence.png", width=3.7, height=3.8, units="in")




#############################################
#############################################
#############################################
######### RAW DATA FIGURE ###################
#############################################

# Now that the modelling is more or less done and paints a picture
# make a quick figure showing the 'raw relationships' (i.e., raw data)
# for the most important traits
# I think this will help structure the paper and show the reader the raw data, 
# instead of just summarized/modelled relationships
# it will also help set up the structure of the paper and what will be talked
# about throughout results/discussion
# I'll choose:
# 1) average number of flight months
# 2) number of adult food types
# 3) hostplant index
# 4) mean temperature in range

raw_plot_dat <- model_data %>%
  dplyr::select(urban_score_mean, flight_months_average,
                temp_mean, hostplant_index_log10, adult_food_types_log10)

ggplot(raw_plot_dat, aes(x=flight_months_average, y=urban_score_mean))+
  geom_point()+
  xlab("Average number of flight months")+
  ylab("Urban affinity score")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm", color="#D95F02") -> flight
flight

ggplot(raw_plot_dat, aes(x=temp_mean, y=urban_score_mean))+
  geom_point()+
  xlab("Mean temperature in range")+
  ylab("")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm", color="#D95F02") -> mean_temp
mean_temp

ggplot(raw_plot_dat, aes(x=10^adult_food_types_log10, y=urban_score_mean))+
  geom_point()+
  xlab("Number of adult food types (log10)")+
  ylab("Urban affinity score")+
  theme_bw()+
  scale_x_log10()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm", color="#D95F02") -> food
food

ggplot(raw_plot_dat, aes(x=10^hostplant_index_log10, y=urban_score_mean))+
  geom_point()+
  xlab("Hostplant specialism index (log10)")+
  ylab("")+
  theme_bw()+
  scale_x_log10()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm", color="#D95F02") -> hostplant_index
hostplant_index

flight + hostplant_index + food + mean_temp

#ggsave("Figures/raw_data_key_plots.png", height=6.7, width=6.8, units="in")
