# This script is to perform the occupancy models
# That is complementary to the overall approach that
# is taking the median of these distributions for each species


library(tidyverse)
library(mgcv)

### get species data ######################

#get list of all species
speciesFiles <- list.files("Data/occupancy_model_data/species_dat_5k")

#read in each file and aggregate across months
speciesData <- plyr::ldply(speciesFiles,function(x){
  temp <- readRDS(paste("Data/occupancy_model_data/species_dat_5k/",x,sep="/"))
  temp %>%
          group_by(grid_id,species) %>%
          summarise(present = max(present,na.rm=T),totalN = sum(N)) %>%
          mutate(present = ifelse(is.infinite(present),0,present))
})

### subset to species in analysis ##########
### And do some taxonomic fixing...

dat <- readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::select(-temp_range) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli")

# get a list of species to run the analysis for
species_list <- dat <- readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::select(-temp_range) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli") %>%
  dplyr::select(species)

length(unique(speciesData$species))

species_in_data <- speciesData %>%
  dplyr::select(species) %>%
  distinct()

species_missing <- species_list %>%
  dplyr::filter(!species %in% species_in_data$species) %>%
  mutate(gbif_species=c("Colias croceus",
                        "Argynnis aglaja",
                        "Aricia eumedon",
                        "Plebejus optilete",
                        "Plebejus orbitulus",
                        "Polyommatus coridon"))

species_taxonomic_missing_dat <- speciesData %>%
  dplyr::filter(species %in% species_missing$gbif_species) %>%
  rename(gbif_species=species) %>%
  left_join(., species_missing) %>%
  dplyr::select(colnames(speciesData))

# combine the dataframes
speciesData <- speciesData %>%
  dplyr::filter(species %in% dat$species) %>%
  bind_rows(species_taxonomic_missing_dat)

length(unique(speciesData$species))

# Finally got the taxonomic fixes for those few species

### get urban cover data ##################

urbanData <- readRDS("Data/occupancy_model_data/grids_viirs_5.RDS")
urbanData <- dplyr::rename(urbanData, urban_lights = mean)
speciesData <- left_join(speciesData,urbanData,by="grid_id")

### fit gams ##############################

normalizeFun <- function(x){(x-min(x))/(max(x)-min(x))}

speciesGAM <- plyr::ddply(speciesData,"species",function(x){ 

  x$log_urban_lights <- log10(x$urban_lights)
  x$n_log_urban_lights <- normalizeFun(x$log_urban_lights)

  #might change the k settings in the line below
  # set wiggliness to low with 5
  gam1 <- gam(present ~ s(n_log_urban_lights,k=5) + log(totalN),
            data = x,
            family = binomial)

  predDF <- data.frame(n_log_urban_lights = seq(0,1,length.out=20),
                       totalN = 5)#5 is the median
  predDF$pred <- predict(gam1,newdata = predDF, type = "response")
  predDF$c_pred <- predDF$pred - median(predDF$pred)
  predDF$s_pred <- predDF$c_pred/sd(predDF$pred)
  
  return(predDF)
  
})

### plot gams #############################

ggplot(subset(speciesGAM,species %in% sample(unique(speciesGAM$species),50)))+
  geom_line(aes(x = n_log_urban_lights,
                y = pred))+
  facet_wrap(~species,scales = "free_y")

#check one obvious urban species
ggplot(subset(speciesGAM,species == "Pieris rapae"))+
  geom_line(aes(x = n_log_urban_lights,
                y = pred))

### dissimilarity matrix #################

speciesGAM_spread <- reshape2::acast(speciesGAM,
                                      species ~ n_log_urban_lights,
                                      value.var = "pred")

library(TSclust)
#use "COR" correlation similarity measure - so only matters where species is relatively more or less common, not absolute values
IP.dis <- diss(speciesGAM_spread, "COR")

### cluster evaluation ###################

#functions 

getClusterStats <- function(mydiss,myclustering){
  require(fpc)
  clustDF <- cluster.stats(mydiss, 
                           clustering=myclustering)
  data.frame(minCluster=clustDF$min.cluster.size,
             avBetween=clustDF$average.between,
             avWithin=clustDF$average.within,
             silWidth=clustDF$avg.silwidth,
             dunn=clustDF$dunn,
             sepIndex=clustDF$sindex)
}

plotTSclust <- function(mylist){
  mylistMelt <- reshape2::melt(mylist,id="Cluster")
  qplot(x=Cluster,y=value,data=mylistMelt)+
    facet_wrap(~variable,scales="free")
}

#compare stats over the range of cluster numbers:
IP.clus <- list()
for(i in 2:20){
  IP.clus[[(i-1)]] <- pam(IP.dis, k = i)$clustering
}

IP.clusList <- plyr::ldply(IP.clus, function(x){
  getClusterStats(mydiss=IP.dis,
                  myclustering=x)})
IP.clusList$Cluster <- 2:20
plotTSclust(IP.clusList)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))
ggsave("Figures/clusterEvaluation.png")

##########################################################
### decide on cluster number #############################
##########################################################
# Chose 3 based on both the investigation of the plots
# and also based on some biological interpretation to match
# exploiters, avoiders, and adapters literature

nuCluster <- 3

IP.clus <- pam(IP.dis, k = nuCluster)$clustering
table(IP.clus)#number of species in each group

#plot species in each cluster
clusterResults <- data.frame(species=names(IP.clus),
                             cluster=as.numeric(IP.clus))
#saveRDS(clusterResults,file="Results/clusterResults.rds")

clusterResults <- readRDS("Results/clusterResults.rds")
speciesGAM <- left_join(speciesGAM,clusterResults) 

### plot clusters separelty ##############################

#cluster 1 - species most common in high urban
ggplot(subset(speciesGAM,cluster==1)) +
  geom_line(aes(x = n_log_urban_lights, y = pred))+
  facet_wrap(~ species, scales = "free_y")+
  theme_bw()  

#cluster 2 - most common at intermediate
ggplot(subset(speciesGAM,cluster==2)) +
  geom_line(aes(x = n_log_urban_lights, y = pred))+
  facet_wrap(~ species, scales = "free_y")+
  theme_bw()  

#cluster 3 - more common at low
ggplot(subset(speciesGAM,cluster==3)) +
  geom_line(aes(x = n_log_urban_lights, y = pred))+
  facet_wrap(~ species, scales = "free_y")+
  theme_bw()  



### plot clusters together #############

ggplot(speciesGAM) +
  geom_line(aes(x = n_log_urban_lights, y = s_pred, 
                group = species, colour = species))+
  facet_wrap(~ cluster, nrow=1)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(legend.position = "none")+
  ylab("Scaled occupancy probability") + 
  xlab("VIIRS night-lights (normalised)")

#scaling helps better compare species that vary in mean occupancy

### trait mapping #######################
dat <- readRDS("Results/data_for_analysis.RDS") %>%
  dplyr::select(-temp_range) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(species != "Cacyreus marshalli")
clusterResults <- readRDS("Results/clusterResults.rds")
clusterResults <- left_join(clusterResults,dat,by="species")

clusterResults

# plot urban tolerance score
# per each cluster
ggplot(clusterResults,aes(x=as.factor(cluster),y=urban_score_mean))+
  geom_violin()+
  geom_boxplot(width=0.1,fill="lightgrey",outlier.shape = NA)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Urban affinity score")+
  coord_flip()+
  ggtitle("N=158 species")

ggsave("Figures/clusters_and_urban_tolerance_scores.png", width=5.6, height=4.7, units="in")

g1 <- ggplot(clusterResults,aes(x=as.factor(cluster),y=flight_months_average))+
  geom_violin()+
  geom_boxplot(width=0.1,fill="lightgrey",outlier.shape = NA)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Average number of flight months")+
  coord_flip()+
  ggtitle("(e)")
g1
g2 <- ggplot(clusterResults,aes(x=as.factor(cluster),y=hostplant_index))+
  geom_violin()+
  geom_boxplot(width=0.1,fill="lightgrey",outlier.shape = NA)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Hostplant specialism index (log10)")+
  scale_y_log10()+
  coord_flip()+
  ggtitle("(c)")
g2
g3 <- ggplot(clusterResults,aes(x=as.factor(cluster),y=adult_food_types))+
  geom_violin()+
  geom_boxplot(width=0.1,fill="lightgrey",outlier.shape = NA)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Cluster")+
  ylab("Number of adult food types (log10)")+
  scale_y_log10()+
  coord_flip()+
  ggtitle("(b)")
g3
g4 <- ggplot(clusterResults,aes(x=as.factor(cluster),y=temp_mean))+
  geom_violin()+
  geom_boxplot(width=0.1,fill="lightgrey",outlier.shape = NA)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Cluster")+
  ylab("Mean temperature in range")+
  coord_flip()+
  ggtitle("(d)")
g4


### mean pattern in each cluster ###########################
### boostrapping ##########################################

#bootstrap mean values within each cluster - resampling by species
library(boot)

# boostrapping function to obtain resampled mean
bootFun <- function(data, indices) {
  d <- data[indices,] 
  mean(d$pred)
}


# bootstrapping function with 1000 replications
applyBoot <- function(df){
  
  results <- boot(data = df, 
                  statistic = bootFun,
                  R=1000)
  bootCI <- boot.ci(results, type="bca")
  
  #pull out bits we want
  data.frame(meanPred = mean(df$pred),
             lowerPred = bootCI$bca[4],
             upperPred = bootCI$bca[5])
  
}


#apply the above functions
clusterSummary <- speciesGAM %>%
  group_by(cluster,n_log_urban_lights) %>%
  group_modify(~applyBoot(.x),.keep=TRUE)

#plot  
clusters <- clusterSummary %>%
  mutate(cluster=paste0("Cluster ", cluster)) %>%
  ggplot(.)+
  geom_line(aes(x = n_log_urban_lights, y = meanPred))+
  geom_ribbon(aes(x = n_log_urban_lights, ymin = lowerPred, ymax = upperPred), alpha = 0.5)+
  facet_wrap(~cluster,nrow=1)+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=7))+
  xlab("VIIRS night-lights (normalised)")+
  ylab("Mean occupancy probability")+
  ggtitle("(a)")
clusters

ggsave("Figures/clusterMeans.png",width=9,height=3)


# make figure for paper
library(patchwork)

clusters/ (g3 | g2)/ (g4 | g1)

ggsave("Figures/clustering_results.png", width=6.7, height=8.6, units="in")


#### end ###################################################

