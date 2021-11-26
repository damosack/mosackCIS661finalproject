library(tidyverse)
library(raster)
library(dismo)
library(terra)
library(rgdal)
library(dplyr)
library(sf)
library(maptools)
library(SSDM)


#create function to select relevant variables from datasets
seloc <- function(tibble) {
  tibble %>% 
    dplyr::filter(observation_date > "2018-12-31") %>%
    dplyr::select(longitude, latitude, common_name)
    
}

#create function to remove duplicate observations
#at same location within each data set.
#having these separate is handy for data checking
uni <- function(tibble) {
  dupes2 <- duplicated(tibble[, c('longitude', 'latitude')])
  nodupes <- tibble[!dupes2, ] %>% 
    dplyr::select('longitude', 'latitude', 'common_name')
  
  
  return(nodupes)
}


#read in data, select relevant variables, assign to objects
unibrnthr <- read_ebd("observations/brnthr.txt") %>% seloc() %>% uni()
unicanwar <- read_ebd("observations/canwar.txt") %>% seloc() %>% uni()
unicomter <- read_ebd("observations/comter.txt") %>% seloc() %>% uni()
unifiespa <- read_ebd("observations/fiespa.txt") %>% seloc() %>% uni()
unigraspa <- read_ebd("observations/graspa.txt") %>% seloc() %>% uni()
unimarwre <- read_ebd("observations/marwre.txt") %>% seloc() %>% uni()
uniperfal <- read_ebd("observations/perfal.txt") %>% seloc() %>% uni()
unirehwoo <- read_ebd("observations/rehwoo.txt") %>% seloc() %>% uni()
uniwilfly <- read_ebd("observations/wilfly.txt") %>% seloc() %>% uni()
uniwoothr <- read_ebd("observations/woothr.txt") %>% seloc() %>% uni()


#assign data sets without duplicates to new objects
#and whittle to just two columns: long/lat.
unibrnthr <- uni(brnthr)
unicanwar <- uni(canwar)
unicomter <- uni(comter)
unifiespa <- uni(fiespa)
unigraspa <- uni(graspa)
unimarwre <- uni(marwre)
uniperfal <- uni(perfal)
unirehwoo <- uni(rehwoo)
uniwilfly <- uni(wilfly)
uniwoothr <- uni(woothr)

#finding lat long extents
allspec <- bind_rows(unibrnthr, unicanwar, unicomter, unifiespa,
            unigraspa, unimarwre, uniperfal, unirehwoo,
            uniwilfly, uniwoothr)

min(allspec$longitude)
max(allspec$longitude)
min(allspec$latitude)
max(allspec$latitude)

#creating rough shape file for michigan state.
states <- st_read("states/s_11au16.shp")
MI <- states %>% 
  dplyr::filter(STATE %in% c("MI", "WI", "OH", "IN", "IL")) %>%
  st_geometry()

plot((MI))
box()

#creating object with all climate predictor tif files
climateallfiles <- (list.files(path = "climatetiff" ,
                                        pattern = '.', 
                                        full.names=TRUE))
#stacking climate files as raster object
predictors <- stack(climateallfiles)

names(predictors)
raster::plot(predictors)


#importing landcover data, 
landcover <- raster("landcover/gaplf2011lc_v30_mi.tif")
landcover
plot(landcover, main= "Landcover 30x30 resolution")

#importing elevation data
elevationfiles <-  list.files(path = 'elevation',
                         pattern = '.', full.names = TRUE)

#adding elevation data to climate predictors stack
allpredfiles <- c(climateallfiles, elevationfiles)
allpred <- stack(allpredfiles)

#trying to crop down predictor extent to michigan
max.lat <- ceiling(max(allspec$latitude))
min.lat <- floor(min(allspec$latitude))
max.lon <- ceiling(max(allspec$longitude))
min.lon <- floor(min(allspec$longitude))

e <- extent(x = c(min.lon, max.lon, min.lat, max.lat))
testpredictor <- raster("climatetiff/current_2-5arcmin_PETseasonality.tif") %>%
  crop(., e)

raster::plot(testpredictor)
points(unicomter$longitude, unicomter$latitude)

crpdpred <- crop(allpred, e)
raster::plot(crpdpred)


quickplot <- function(tibble) {
  name <- tibble$common_name[1]
  
plot(MI,
     xlim= c(min.lon, max.lon),
     ylim= c(min.lat, max.lat),
     axes= TRUE,
     col='grey95', main = name)
points(x= tibble$longitude,
       y = tibble$latitude,
       col= "olivedrab",
       pch= 20,
       cex= 0.1)
box()
}

#plotting distributions for each species on michigan
#map using function. See directly above.
quickplot(unibrnthr)
quickplot(unicanwar)
quickplot(unicomter)
quickplot(unifiespa)
quickplot(unigraspa)
quickplot(unimarwre)
quickplot(uniperfal)
quickplot(unirehwoo)
quickplot(uniwilfly)
quickplot(uniwoothr)

#get observation data ready for modelling.
unipbrnthr <- unibrnthr %>% dplyr::select(longitude, latitude)
unipcanwar <- unicanwar %>% dplyr::select(longitude, latitude)
unipcomter <- unicomter %>% dplyr::select(longitude, latitude)
unipfiespa <- unifiespa %>% dplyr::select(longitude, latitude)
unipgraspa <- unigraspa %>% dplyr::select(longitude, latitude)
unipmarwre <- unimarwre %>% dplyr::select(longitude, latitude)
unipperfal <- uniperfal %>% dplyr::select(longitude, latitude)
uniprehwoo <- unirehwoo %>% dplyr::select(longitude, latitude)
unipwilfly <- uniwilfly %>% dplyr::select(longitude, latitude)
unipwoothr <- uniwoothr %>% dplyr::select(longitude, latitude)

#building models
model.brnthr <- bioclim(x= crpdpred, p = unipbrnthr)
model.canwar <- bioclim(x= crpdpred, p = unipcanwar)
model.comter <- bioclim(x= crpdpred, p = unipcomter)
model.fiespa <- bioclim(x= crpdpred, p = unipfiespa)
model.graspa <- bioclim(x= crpdpred, p = unipgraspa)
model.marwre <- bioclim(x= crpdpred, p = unipmarwre)
model.perfal <- bioclim(x= crpdpred, p = unipperfal)
model.rehwoo <- bioclim(x= crpdpred, p = uniprehwoo)
model.wilfly <- bioclim(x= crpdpred, p = unipwilfly)
model.woothr <- bioclim(x= crpdpred, p = unipwoothr)

#generation prediction data based on our models
pred.presence.brnthr <- dismo::predict(object= model.brnthr, x= crpdpred, ext = e)
pred.presence.canwar <- dismo::predict(object= model.canwar, x= crpdpred, ext = e)
pred.presence.comter <- dismo::predict(object= model.comter, x= crpdpred, ext = e)
pred.presence.fiespa <- dismo::predict(object= model.fiespa, x= crpdpred, ext = e)
pred.presence.graspa <- dismo::predict(object= model.graspa, x= crpdpred, ext = e)
pred.presence.marwre <- dismo::predict(object= model.marwre, x= crpdpred, ext = e)
pred.presence.perfal <- dismo::predict(object= model.perfal, x= crpdpred, ext = e)
pred.presence.rehwoo <- dismo::predict(object= model.rehwoo, x= crpdpred, ext = e)
pred.presence.wilfly <- dismo::predict(object= model.wilfly, x= crpdpred, ext = e)
pred.presence.woothr <- dismo::predict(object= model.woothr, x= crpdpred, ext = e)


presenceplot <- function(unitibble, pred.presence.species) {
 
  #build custom title for each map
   name <- unitibble$common_name[1]
  
  #plot base map
  raster::plot(MI,
       xlim= c(min.lon, max.lon),
       ylim= c(min.lat, max.lat),
       axes= TRUE,
       col='grey95', main = name)
  
  #add model probabilities
  raster::plot(pred.presence.species, 
       add= TRUE)
  
  #redraw state border
  raster::plot((MI),
       xlim= c(min.lon, max.lon),
       ylim= c(min.lat, max.lat), 
       add= TRUE, border= "grey5")
  
  #add original observations to map
  points(x= unitibble$longitude,
         y = unitibble$latitude,
         col= "olivedrab",
         pch= 20,
         cex= 0.1)
  box()
}


presenceplot(unibrnthr, pred.presence.brnthr)
presenceplot(unicanwar, pred.presence.canwar)
presenceplot(unicomter, pred.presence.comter)
presenceplot(unifiespa, pred.presence.fiespa)
presenceplot(unigraspa, pred.presence.graspa)
presenceplot(unimarwre, pred.presence.marwre)
presenceplot(uniperfal, pred.presence.perfal)
presenceplot(unirehwoo, pred.presence.rehwoo)
presenceplot(uniwilfly, pred.presence.wilfly)
presenceplot(uniwoothr, pred.presence.woothr)


###Creating Psuedo-absence data

#grabbing a bioclim file for determining spatial resoluton
mask <- raster(allpredfiles[1])
#randomly sampling points, sample = observed n
psuedont <- function(uniptibble) {
  background <- randomPoints(mask=mask,
                             n= nrow(uniptibble),
                             ext= e,
                             extf = 1.25)
  return(background)
} 

#setting a seed for rng, picked by rng
set.seed(1792)
#generating psuedoabsence data for all species distributions
fabsbrnthr <- psuedont(unipbrnthr)
fabscanwar <- psuedont(unipcanwar)
fabscomter <- psuedont(unipcomter)
fabsfiespa <- psuedont(unipfiespa)
fabsgraspa <- psuedont(unipgraspa)
fabsmarwre <- psuedont(unipmarwre)
fabsperfal <- psuedont(unipperfal)
fabsrehwoo <- psuedont(uniprehwoo)
fabswilfly <- psuedont(unipwilfly)
fabswoothr <- psuedont(unipwoothr)

#formula to plot pres/abs data on midwest shapefile:
presabsplot <- function(unitibble, fabstibble) {
  
  #build custom title for each map
  name <- c(unitibble$common_name[1], "presence and pseudo-absences points")
  
  #plot base map
  raster::plot(MI,
               xlim= c(min.lon, max.lon),
               ylim= c(min.lat, max.lat),
               axes= TRUE,
               col='grey95', main = name)
  
  #add original observations to map
  points(x= unitibble$longitude,
         y = unitibble$latitude,
         col= "olivedrab",
         pch= 20,
         cex= 0.1)
  #add pseudo-absence data
  points(fabstibble, col = "grey30", pch=1, cex= 0.1)
  box()
}

#testing
head(fabsbrnthr)
#plotting pres/abs data
presabsplot(unibrnthr, fabsbrnthr)
presabsplot(unicanwar, fabscanwar)
presabsplot(unicomter, fabscomter)
presabsplot(unifiespa, fabsfiespa)
presabsplot(unigraspa, fabsgraspa)
presabsplot(unimarwre, fabsmarwre)
presabsplot(uniperfal, fabsperfal)
presabsplot(unirehwoo, fabsrehwoo)
presabsplot(uniwilfly, fabswilfly)
presabsplot(uniwoothr, fabswoothr)
 
#assign group 1 as testing data group (arbitrary)
testing.group <- 1
set.seed(2741)

#create vector of group memberships
#function to do this:
groupPresence <- function(uniptibble) {
  dismo::kfold(x= uniptibble, k=5)
}
#and now use function for all uniptibbles
gpbrnthr <- groupPresence(unibrnthr)
gpcanwar <- groupPresence(unicanwar)
gpcomter <- groupPresence(unicomter)
gpfiespa <- groupPresence(unifiespa)
gpgraspa <- groupPresence(unigraspa)
gpmarwre <- groupPresence(unimarwre)
gpperfal <- groupPresence(uniperfal)
gprehwoo <- groupPresence(unirehwoo)
gpwilfly <- groupPresence(uniwilfly)
gpwoothr <- groupPresence(uniwoothr)

#checking output, head(gpbrnthr) should be c(3, 3, 5, 2, 5, 1)
#table should show even split of observations
head(gpbrnthr)
table(gpbrnthr)

#separate observations into training and testing groups
ptrainbrnthr <- unipbrnthr[gpbrnthr != testing.group,]
ptraincanwar <- unipcanwar[gpcanwar != testing.group,]
ptraincomter <- unipcomter[gpcomter != testing.group,]
ptrainfiespa <- unipfiespa[gpfiespa != testing.group,]
ptraingraspa <- unipgraspa[gpgraspa != testing.group,]
ptrainmarwre <- unipmarwre[gpmarwre != testing.group,]
ptrainperfal <- unipperfal[gpperfal != testing.group,]
ptrainrehwoo <- uniprehwoo[gprehwoo != testing.group,]
ptrainwilfly <- unipwilfly[gpwilfly != testing.group,]
ptrainwoothr <- unipwoothr[gpwoothr != testing.group,]

ptestbrnthr <- unipbrnthr[gpbrnthr == testing.group, ]
ptestcanwar <- unipcanwar[gpcanwar == testing.group, ]
ptestcomter <- unipcomter[gpcomter == testing.group, ]
ptestfiespa <- unipfiespa[gpfiespa == testing.group, ]
ptestgraspa <- unipgraspa[gpgraspa == testing.group, ]
ptestmarwre <- unipmarwre[gpmarwre == testing.group, ]
ptestperfal <- unipperfal[gpperfal == testing.group, ]
ptestrehwoo <- uniprehwoo[gprehwoo == testing.group, ]
ptestwilfly <- unipwilfly[gpwilfly == testing.group, ]
ptestwoothr <- unipwoothr[gpwoothr == testing.group, ]

#separate background pseudo-absence data into train and test groups 
gbbrnthr <- kfold(x= fabsbrnthr, k= 5)
gbcanwar <- kfold(x= fabscanwar, k= 5)
gbcomter <- kfold(x= fabscomter, k= 5)
gbfiespa <- kfold(x= fabsfiespa, k= 5)
gbgraspa <- kfold(x= fabsgraspa, k= 5)
gbmarwre <- kfold(x= fabsmarwre, k= 5)
gbperfal <- kfold(x= fabsperfal, k= 5)
gbrehwoo <- kfold(x= fabsrehwoo, k= 5)
gbwilfly <- kfold(x= fabswilfly, k= 5)
gbwoothr <- kfold(x= fabswoothr, k= 5)

btrainbrnthr <- fabsbrnthr[gbbrnthr != testing.group, ]
btraincanwar <- fabscanwar[gbcanwar != testing.group, ]
btraincomter <- fabscomter[gbcomter != testing.group, ]
btrainfiespa <- fabsfiespa[gbfiespa != testing.group, ]
btraingraspa <- fabsgraspa[gbgraspa != testing.group, ]
btrainmarwre <- fabsmarwre[gbmarwre != testing.group, ]
btrainperfal <- fabsperfal[gbperfal != testing.group, ]
btrainrehwoo <- fabsrehwoo[gbrehwoo != testing.group, ]
btrainwilfly <- fabswilfly[gbwilfly != testing.group, ]
btrainwoothr <- fabswoothr[gbwoothr != testing.group, ]

btestbrnthr <- fabsbrnthr[gbbrnthr == testing.group, ]
btestcanwar <- fabscanwar[gbcanwar == testing.group, ]
btestcomter <- fabscomter[gbcomter == testing.group, ]
btestfiespa <- fabsfiespa[gbfiespa == testing.group, ]
btestgraspa <- fabsgraspa[gbgraspa == testing.group, ]
btestmarwre <- fabsmarwre[gbmarwre == testing.group, ]
btestperfal <- fabsperfal[gbperfal == testing.group, ]
btestrehwoo <- fabsrehwoo[gbrehwoo == testing.group, ]
btestwilfly <- fabswilfly[gbwilfly == testing.group, ]
btestwoothr <- fabswoothr[gbwoothr == testing.group, ]

#create new models with training data only
bc.model.brnthr <- bioclim(x= crpdpred, p=ptrainbrnthr)
bc.model.canwar <- bioclim(x= crpdpred, p=ptraincanwar)
bc.model.comter <- bioclim(x= crpdpred, p=ptraincomter)
bc.model.fiespa <- bioclim(x= crpdpred, p=ptrainfiespa)
bc.model.graspa <- bioclim(x= crpdpred, p=ptraingraspa)
bc.model.marwre <- bioclim(x= crpdpred, p=ptrainmarwre)
bc.model.perfal <- bioclim(x= crpdpred, p=ptrainperfal)
bc.model.rehwoo <- bioclim(x= crpdpred, p=ptrainrehwoo)
bc.model.wilfly <- bioclim(x= crpdpred, p=ptrainwilfly)
bc.model.woothr <- bioclim(x= crpdpred, p=ptrainwoothr)

#use models from training data to predict suitable habitat, similar as done above
pred.presence.brnthr <- dismo::predict(object= bc.model.brnthr, x= crpdpred, ext= e)
pred.presence.canwar <- dismo::predict(object= bc.model.canwar, x= crpdpred, ext= e)
pred.presence.comter <- dismo::predict(object= bc.model.comter, x= crpdpred, ext= e)
pred.presence.fiespa <- dismo::predict(object= bc.model.fiespa, x= crpdpred, ext= e)
pred.presence.graspa <- dismo::predict(object= bc.model.graspa, x= crpdpred, ext= e)
pred.presence.marwre <- dismo::predict(object= bc.model.marwre, x= crpdpred, ext= e)
pred.presence.perfal <- dismo::predict(object= bc.model.perfal, x= crpdpred, ext= e)
pred.presence.rehwoo <- dismo::predict(object= bc.model.rehwoo, x= crpdpred, ext= e)
pred.presence.wilfly <- dismo::predict(object= bc.model.wilfly, x= crpdpred, ext= e)
pred.presence.woothr <- dismo::predict(object= bc.model.woothr, x= crpdpred, ext= e)
#use data set aside as test.data to evaluate model
bc.eval.brnthr <- evaluate(p = ptestbrnthr, a = btestbrnthr, model = bc.model.brnthr, x = crpdpred)
bc.eval.canwar <- evaluate(p = ptestcanwar, a = btestcanwar, model = bc.model.canwar, x = crpdpred)
bc.eval.comter <- evaluate(p = ptestcomter, a = btestcomter, model = bc.model.comter, x = crpdpred)
bc.eval.fiespa <- evaluate(p = ptestfiespa, a = btestfiespa, model = bc.model.fiespa, x = crpdpred)
bc.eval.graspa <- evaluate(p = ptestgraspa, a = btestgraspa, model = bc.model.graspa, x = crpdpred)
bc.eval.marwre <- evaluate(p = ptestmarwre, a = btestmarwre, model = bc.model.marwre, x = crpdpred)
bc.eval.perfal <- evaluate(p = ptestperfal, a = btestperfal, model = bc.model.perfal, x = crpdpred)
bc.eval.rehwoo <- evaluate(p = ptestrehwoo, a = btestrehwoo, model = bc.model.rehwoo, x = crpdpred)
bc.eval.wilfly <- evaluate(p = ptestwilfly, a = btestwilfly, model = bc.model.wilfly, x = crpdpred)
bc.eval.woothr <- evaluate(p = ptestwoothr, a = btestwoothr, model = bc.model.woothr, x = crpdpred)

#Determine minimum threshold for "presence"
#"spec_sens" sets the threshold at which the sum of the sensitivity (TPR)
#and the specificity (TNR) is highest
bc.thresh.brnthr <- threshold(x= bc.eval.brnthr, stat = "spec_sens")
bc.thresh.canwar <- threshold(x= bc.eval.canwar, stat = "spec_sens")
bc.thresh.comter <- threshold(x= bc.eval.comter, stat = "spec_sens")
bc.thresh.fiespa <- threshold(x= bc.eval.fiespa, stat = "spec_sens")
bc.thresh.graspa <- threshold(x= bc.eval.graspa, stat = "spec_sens")
bc.thresh.marwre <- threshold(x= bc.eval.marwre, stat = "spec_sens")
bc.thresh.perfal <- threshold(x= bc.eval.perfal, stat = "spec_sens")
bc.thresh.rehwoo <- threshold(x= bc.eval.rehwoo, stat = "spec_sens")
bc.thresh.wilfly <- threshold(x= bc.eval.wilfly, stat = "spec_sens")
bc.thresh.woothr <- threshold(x= bc.eval.woothr, stat = "spec_sens")

plot.thresh.pred <- function(unitibble, pred.presence, bc.thresh) {
  #build custom title for each map
  name <- c(unitibble$common_name[1],
            "threshold predicted range",
            "where model achieves max sum(TPR+TNR)")
  
  #plot base map
  raster::plot(MI,
               xlim= c(min.lon, max.lon),
               ylim= c(min.lat, max.lat),
               axes= TRUE,
               col='grey95',
               main = name)
  #only plot areas where probability of occurrence
  #is greater than the threshold.
  raster::plot(pred.presence > bc.thresh,
       add = TRUE,
       legend= FALSE,
       col= c(NA, "olivedrab"))
  
  #add original observations to map
  points(x= unitibble$longitude,
         y = unitibble$latitude,
         col= "black",
         pch= 20,
         cex= 0.1)
 
  raster::plot(MI,
               add=TRUE,
               boder="grey5")
  box()
}

plot.thresh.pred(unibrnthr, pred.presence.brnthr, bc.thresh.brnthr)
plot.thresh.pred(unicanwar, pred.presence.canwar, bc.thresh.canwar)
plot.thresh.pred(unicomter, pred.presence.comter, bc.thresh.comter)
plot.thresh.pred(unifiespa, pred.presence.fiespa, bc.thresh.fiespa)
plot.thresh.pred(unigraspa, pred.presence.graspa, bc.thresh.graspa)
plot.thresh.pred(unimarwre, pred.presence.marwre, bc.thresh.marwre)
plot.thresh.pred(uniperfal, pred.presence.perfal, bc.thresh.perfal)
plot.thresh.pred(unirehwoo, pred.presence.rehwoo, bc.thresh.rehwoo)
plot.thresh.pred(uniwilfly, pred.presence.wilfly, bc.thresh.wilfly)
plot.thresh.pred(uniwoothr, pred.presence.woothr, bc.thresh.woothr)

#isolating the sdm layer so I can export plots as tiff
#files which hopefully can be assembled into SSDMs
SDMlayer <- function(pred.presence, bc.thresh){
raster::plot(pred.presence > bc.thresh,
             
             legend= FALSE,
            )
}

SDMlayer(pred.presence.brnthr, bc.thresh.brnthr)
SDMlayer(pred.presence.canwar, bc.thresh.canwar)
SDMlayer(pred.presence.comter, bc.thresh.comter)
SDMlayer(pred.presence.fiespa, bc.thresh.fiespa)
SDMlayer(pred.presence.graspa, bc.thresh.graspa)
SDMlayer(pred.presence.marwre, bc.thresh.marwre)
SDMlayer(pred.presence.perfal, bc.thresh.perfal)
SDMlayer(pred.presence.rehwoo, bc.thresh.rehwoo)
SDMlayer(pred.presence.wilfly, bc.thresh.wilfly)
SDMlayer(pred.presence.woothr, bc.thresh.woothr)

#created new folder "SDM_pred_ranges", containing
#all SDM plots (range layer) as tiff files
SDMfiles <- list.files(path= "SDM_pred_ranges",
                       pattern= '.',
                       full.names =  TRUE)
SDMs <- stack(SDMfiles)
SDMs
SSDM <- stackApply(SDMs, indices = c(1,1,1,1,1,1,1,1,1,1), fun= sum)
plot(SSDM)


















