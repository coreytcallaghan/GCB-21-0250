library(tidyverse)

### get data ######################

#get list of all species
speciesFiles <- list.files("Data/occupancy_model_data/species_dat_5k")

#get urban cover data
urbanData <- readRDS("Data/occupancy_model_data/grids_viirs_5.RDS")
urbanData <- rename(urbanData, urban_lights = mean)

### example for one species #######

#read in an example species file
out <- readRDS(paste("Data/occupancy_model_data/species_dat_5k",speciesFiles[1],sep="/"))

#sum across multiple values per month
out <- out %>%
  group_by(grid_id,week,species) %>%
  summarise(present = sum(present,na.rm=T),N = sum(N,na.rm=T))

### flight period ##################

outPresent <- subset(out, present==1)
hist(outPresent$week)
summary(outPresent$week)

#subset with main flight period
out <- subset(out,
              week > quantile(outPresent$week,0.25) &
              week < quantile(outPresent$week,0.75))

### merge with species data ########

outMerged <- left_join(out,urbanData)

### format for unmarked ###########

library(unmarked)
library(reshape2)

#organise the observation data with sites as rows and weeks (repeat surveys as months)

yData <- acast(outMerged,grid_id~week,value.var="present")

#organise urban cover data for occurrence model
urbanCovariate <- distinct(outMerged[,c("grid_id","urban_lights")])
dim(yData)[1]==nrow(urbanCovariate)

#organise urban cover and effort for detection model
urbanDetection <- matrix(urbanCovariate$urban_lights, dim(yData)[1], dim(yData)[2],
                         byrow=FALSE)

effortDetection <- acast(outMerged,grid_id~week,value.var="N")
effortDetection[is.na(effortDetection)] <- 0

umf <- unmarkedFrameOccu(
y = yData,                                        
siteCovs = data.frame(urban_lights = urbanCovariate$urban_lights),  
obsCovs = list(urban_detection = urbanDetection,
               effort_detection = effortDetection))         
summary(umf)

### fit model ######################

occModel <- occu(~ effort_detection + urban_detection ~urban_lights, data=umf)


### summarise model ################

summary(occModel)

urbanResponse <- occModel@estimates@estimates$state@estimates[2]

### as function ####################

fitOccModel <- function(file){

out <- readRDS(paste("Data/occupancy_model_data/species_dat_5k",file,sep="/"))

mySpecies <- out$species[1]

#sum across multiple values per month
out <- out %>%
  group_by(grid_id,week,species) %>%
  summarise(present = sum(present,na.rm=T),N = sum(N,na.rm=T))


#subset with main flight period
outPresent <- subset(out, present==1)
out <- subset(out,
              week > quantile(outPresent$week,0.25) &
                week < quantile(outPresent$week,0.75))

out$month <- lubridate::month(as.Date(paste(2019, out$week, 1, sep="-"), "%Y-%U-%u"))

outMerged <- left_join(out,urbanData)

#organise the observation data with sites as rows and weeks (repeat surveys as months)
yData <- acast(outMerged,grid_id~week,value.var="present")

#organise urban cover data for occurrence model
urbanCovariate <- distinct(outMerged[,c("grid_id","urban_lights")])

#organise urban cover and effort for detection model
urbanDetection <- matrix(urbanCovariate$urban_lights, dim(yData)[1], dim(yData)[2],
                         byrow=FALSE)

effortDetection <- acast(outMerged,grid_id~week,value.var="N")
effortDetection[is.na(effortDetection)] <- 0

#run in unmarked
umf <- unmarkedFrameOccu(
  y = yData,                                        
  siteCovs = data.frame(urban_lights = log(urbanCovariate$urban_lights+1)),  
  obsCovs = list(urban_detection = log(urbanDetection+1),
                 effort_detection = log(effortDetection+1)))         
occModel <- occu(~ effort_detection + urban_detection ~urban_lights, data=umf)

#extract components
temp <- summary(occModel)
urbanResponse <- temp$state$Estimate[2]
urbanResponse_SE <- temp$state$SE[2]

return(data.frame(Species = mySpecies, 
                  urbanResponse,
                  urbanResponse_SE))

}

### apply to each species ##################

fitOccModel(file)

output <- plyr::ldply(speciesFiles,fitOccModel)

saveRDS(output,file="Results/occ_models/output_occmodels.rds")

### plot a sample #########################

tempS <- output[sample(nrow(output),50),]
tempS <- arrange(tempS,urbanResponse)
tempS$Species <- factor(tempS$Species,levels=tempS$Species)
ggplot(tempS)+
  geom_point(aes(x=Species,y=urbanResponse))+
  coord_flip()+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")


