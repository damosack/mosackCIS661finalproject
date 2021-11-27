library(tidyverse)
library(raster)
library(dismo)
library(terra)
library(rgdal)
library(dplyr)
library(sf)
library(maptools)
library(viridis)
library(auk)


#create function to select relevant variables from datasets
seloc <- function(tibble) {
  tibble %>% 
    dplyr::filter(observation_date > "2017-12-31") %>%
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
#add prefix "uni" bc these are unique entries for each species
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
unihenspa <- read_ebd("observations/henspa.txt") %>% seloc() %>% uni()
unihorgre <- read_ebd("observations/horgre.txt") %>% seloc() %>% uni()
unikenwar <- read_ebd("observations/kenwar.txt") %>% seloc() %>% uni()
unileabit <- read_ebd("observations/leabit.txt") %>% seloc() %>% uni()
unilogshr <- read_ebd("observations/logshr.txt") %>% seloc() %>% uni()
unipibgre <- read_ebd("observations/pibgre.txt") %>% seloc() %>% uni()
unirusbla <- read_ebd("observations/rusbla.txt") %>% seloc() %>% uni()
unisheowl <- read_ebd("observations/sheowl.txt") %>% seloc() %>% uni()
uniyelrai <- read_ebd("observations/yelrai.txt") %>% seloc() %>% uni()


#finding lat long extents, assigning them to objects
allspec <- bind_rows(unibrnthr, unicanwar, unicomter, unifiespa,
            unigraspa, unimarwre, uniperfal, unirehwoo,
            uniwilfly, uniwoothr)

min.lon <- floor(min(allspec$longitude))
max.lon <- ceiling(max(allspec$longitude))
min.lat <- floor(min(allspec$latitude))
max.lat <- ceiling(max(allspec$latitude))
                 
#creating object, e, to store lat/long extent data for cropping later
e <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#creating rough shape file for michigan state.
states <- st_read("states/s_11au16.shp")
MI <- states %>% 
  dplyr::filter(STATE %in% c("MI"))

plot(st_geometry(MI))
box()

#creating object with all climate predictor tif files
climateallfiles <- (list.files(path = "climatetiff" ,
                                        pattern = '.', 
                                        full.names=TRUE))
#stacking climate files as raster object
predictors <- stack(climateallfiles)

names(predictors)



#importing landcover data, not used for now bc wrong resolution
#could try to resample resolution to get to 1/24 x 1/24
#which is what I believe the other predictor files are in
landcover <- raster("landcover/gaplf2011lc_v30_mi.tif")
landcover
plot(landcover, main= "Landcover 30x30 resolution")

#importing elevation data
elevationfiles <-  list.files(path = 'elevation',
                         pattern = '.', full.names = TRUE)

#adding elevation data to climate predictors stack
allpredfiles <- c(climateallfiles, elevationfiles)
allpred <- stack(allpredfiles)




testpredictor <- terra::mask(x=raster("climatetiff/current_2-5arcmin_PETseasonality.tif"), MI) %>%
  crop(., e)

raster::plot(testpredictor)
points(unicomter$longitude, unicomter$latitude)

cropped <- crop(allpred, e)

crpdpred <- terra::mask(cropped, MI)

raster::plot(crpdpred)


quickplot <- function(tibble) {
  name <- tibble$common_name[1]
  
plot(st_geometry(MI),
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
quickplot(unihenspa)
quickplot(unihorgre)
quickplot(unikenwar)
quickplot(unileabit)
quickplot(unilogshr)
quickplot(unipibgre)
quickplot(unirusbla)
quickplot(unisheowl)
quickplot(uniyelrai)

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
uniphenspa <- unihenspa %>% dplyr::select(longitude, latitude)
uniphorgre <- unihorgre %>% dplyr::select(longitude, latitude)
unipkenwar <- unikenwar %>% dplyr::select(longitude, latitude)
unipleabit <- unileabit %>% dplyr::select(longitude, latitude)
uniplogshr <- unilogshr %>% dplyr::select(longitude, latitude)
unippibgre <- unipibgre %>% dplyr::select(longitude, latitude)
uniprusbla <- unirusbla %>% dplyr::select(longitude, latitude)
unipsheowl <- unisheowl %>% dplyr::select(longitude, latitude)
unipyelrai <- uniyelrai %>% dplyr::select(longitude, latitude)

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
model.henspa <- bioclim(x= crpdpred, p = uniphenspa)
model.horgre <- bioclim(x= crpdpred, p = uniphorgre)
model.kenwar <- bioclim(x= crpdpred, p = unipkenwar)
model.leabit <- bioclim(x= crpdpred, p = unipleabit)
model.logshr <- bioclim(x= crpdpred, p = uniplogshr)
model.pibgre <- bioclim(x= crpdpred, p = unippibgre)
model.rusbla <- bioclim(x= crpdpred, p = uniprusbla)
model.sheowl <- bioclim(x= crpdpred, p = unipsheowl)
model.yelrai <- bioclim(x= crpdpred, p = unipyelrai)

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
pred.presence.henspa <- dismo::predict(object= model.henspa, x= crpdpred, ext = e)
pred.presence.horgre <- dismo::predict(object= model.horgre, x= crpdpred, ext = e)
pred.presence.kenwar <- dismo::predict(object= model.kenwar, x= crpdpred, ext = e)
pred.presence.leabit <- dismo::predict(object= model.leabit, x= crpdpred, ext = e)
pred.presence.logshr <- dismo::predict(object= model.logshr, x= crpdpred, ext = e)
pred.presence.pibgre <- dismo::predict(object= model.pibgre, x= crpdpred, ext = e)
pred.presence.rusbla <- dismo::predict(object= model.rusbla, x= crpdpred, ext = e)
pred.presence.sheowl <- dismo::predict(object= model.sheowl, x= crpdpred, ext = e)
pred.presence.yelrai <- dismo::predict(object= model.yelrai, x= crpdpred, ext = e)

presenceplot <- function(unitibble, pred.presence.species) {
 
  #build custom title for each map
   name <- unitibble$common_name[1]
  
  #plot base map
  raster::plot(st_geometry(MI),
       xlim= c(min.lon, max.lon),
       ylim= c(min.lat, max.lat),
       axes= TRUE,
       col='grey95', main = name)
  
  #add model probabilities
  raster::plot(pred.presence.species, 
       add= TRUE)
  
  #redraw state border
  raster::plot(st_geometry(MI),
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
presenceplot(unihenspa, pred.presence.henspa)
presenceplot(unihorgre, pred.presence.horgre)
presenceplot(unikenwar, pred.presence.kenwar)
presenceplot(unileabit, pred.presence.leabit)
presenceplot(unilogshr, pred.presence.logshr)
presenceplot(unipibgre, pred.presence.pibgre)
presenceplot(unirusbla, pred.presence.rusbla)
presenceplot(unisheowl, pred.presence.sheowl)
presenceplot(uniyelrai, pred.presence.yelrai)


###Creating Psuedo-absence data

#grabbing a pre-cropped and masked bioclim file
#for determining spatial resoluton
mask= testpredictor
#randomly sampling points, sample = observed n
pseudont <- function(uniptibble) {
  background <- randomPoints(mask=mask,
                             n= nrow(uniptibble),
                             ext= e,
                             extf = 1.25)
  return(background)
} 

#setting a seed for rng, picked by rng
set.seed(1792)
#generating pseudoabsence data for all species distributions
fabsbrnthr <- pseudont(unipbrnthr)
fabscanwar <- pseudont(unipcanwar)
fabscomter <- pseudont(unipcomter)
fabsfiespa <- pseudont(unipfiespa)
fabsgraspa <- pseudont(unipgraspa)
fabsmarwre <- pseudont(unipmarwre)
fabsperfal <- pseudont(unipperfal)
fabsrehwoo <- pseudont(uniprehwoo)
fabswilfly <- pseudont(unipwilfly)
fabswoothr <- pseudont(unipwoothr)
fabshenspa <- pseudont(uniphenspa)
fabshorgre <- pseudont(uniphorgre)
fabskenwar <- pseudont(unipkenwar)
fabsleabit <- pseudont(unipleabit)
fabslogshr <- pseudont(uniplogshr)
fabspibgre <- pseudont(unippibgre)
fabsrusbla <- pseudont(uniprusbla)
fabssheowl <- pseudont(unipsheowl)
fabsyelrai <- pseudont(unipyelrai)

#formula to plot pres/abs data on midwest shapefile:
presabsplot <- function(unitibble, fabstibble) {
  
  #build custom title for each map
  name <- c(unitibble$common_name[1], "presence and pseudo-absences points")
  
  #plot base map
  raster::plot(st_geometry(MI),
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
presabsplot(unihenspa, fabshenspa)
presabsplot(unihorgre, fabshorgre)
presabsplot(unikenwar, fabskenwar)
presabsplot(unileabit, fabsleabit)
presabsplot(unilogshr, fabslogshr)
presabsplot(unipibgre, fabspibgre)
presabsplot(unirusbla, fabsrusbla)
presabsplot(unisheowl, fabssheowl)
presabsplot(uniyelrai, fabsyelrai) 

#assign group 1 as testing data group (arbitrary)
testing.group <- 1
set.seed(2741)

#create vector of group memberships
#function to do this:
groupPresence <- function(uniptibble) {
  dismo::kfold(x= uniptibble, k=5)
}
#and now use function for all uniptibbles
gpbrnthr <- groupPresence(unipbrnthr)
gpcanwar <- groupPresence(unipcanwar)
gpcomter <- groupPresence(unipcomter)
gpfiespa <- groupPresence(unipfiespa)
gpgraspa <- groupPresence(unipgraspa)
gpmarwre <- groupPresence(unipmarwre)
gpperfal <- groupPresence(unipperfal)
gprehwoo <- groupPresence(uniprehwoo)
gpwilfly <- groupPresence(unipwilfly)
gpwoothr <- groupPresence(unipwoothr)
gphenspa <- groupPresence(uniphenspa)
gphorgre <- groupPresence(uniphorgre)
gpkenwar <- groupPresence(unipkenwar)
gpleabit <- groupPresence(unipleabit)
gplogshr <- groupPresence(uniplogshr)
gppibgre <- groupPresence(unippibgre)
gprusbla <- groupPresence(uniprusbla)
gpsheowl <- groupPresence(unipsheowl)
gpyelrai <- groupPresence(unipyelrai)

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
ptrainhenspa <- uniphenspa[gphenspa != testing.group,]
ptrainhorgre <- uniphorgre[gphorgre != testing.group,]
ptrainkenwar <- unipkenwar[gpkenwar != testing.group,]
ptrainleabit <- unipleabit[gpleabit != testing.group,]
ptrainlogshr <- uniplogshr[gplogshr != testing.group,]
ptrainpibgre <- unippibgre[gppibgre != testing.group,]
ptrainrusbla <- uniprusbla[gprusbla != testing.group,]
ptrainsheowl <- unipsheowl[gpsheowl != testing.group,]
ptrainyelrai <- unipyelrai[gpyelrai != testing.group,]

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
ptesthenspa <- uniphenspa[gphenspa == testing.group, ]
ptesthorgre <- uniphorgre[gphorgre == testing.group, ]
ptestkenwar <- unipkenwar[gpkenwar == testing.group, ]
ptestleabit <- unipleabit[gpleabit == testing.group, ]
ptestlogshr <- uniplogshr[gplogshr == testing.group, ]
ptestpibgre <- unippibgre[gppibgre == testing.group, ]
ptestrusbla <- uniprusbla[gprusbla == testing.group, ]
ptestsheowl <- unipsheowl[gpsheowl == testing.group, ]
ptestyelrai <- unipyelrai[gpyelrai == testing.group, ]

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
gbhenspa <- kfold(x= fabshenspa, k= 5)
gbhorgre <- kfold(x= fabshorgre, k= 5)
gbkenwar <- kfold(x= fabskenwar, k= 5)
gbleabit <- kfold(x= fabsleabit, k= 5)
gblogshr <- kfold(x= fabslogshr, k= 5)
gbpibgre <- kfold(x= fabspibgre, k= 5)
gbrusbla <- kfold(x= fabsrusbla, k= 5)
gbsheowl <- kfold(x= fabssheowl, k= 5)
gbyelrai <- kfold(x= fabsyelrai, k= 5)

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
btrainhenspa <- fabshenspa[gbhenspa != testing.group, ]
btrainhorgre <- fabshorgre[gbhorgre != testing.group, ]
btrainkenwar <- fabskenwar[gbkenwar != testing.group, ]
btrainleabit <- fabsleabit[gbleabit != testing.group, ]
btrainlogshr <- fabslogshr[gblogshr != testing.group, ]
btrainpibgre <- fabspibgre[gbpibgre != testing.group, ]
btrainrusbla <- fabsrusbla[gbrusbla != testing.group, ]
btrainsheowl <- fabssheowl[gbsheowl != testing.group, ]
btrainyelrai <- fabsyelrai[gbyelrai != testing.group, ]

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
btesthenspa <- fabshenspa[gbhenspa == testing.group, ]
btesthorgre <- fabshorgre[gbhorgre == testing.group, ]
btestkenwar <- fabskenwar[gbkenwar == testing.group, ]
btestleabit <- fabsleabit[gbleabit == testing.group, ]
btestlogshr <- fabslogshr[gblogshr == testing.group, ]
btestpibgre <- fabspibgre[gbpibgre == testing.group, ]
btestrusbla <- fabsrusbla[gbrusbla == testing.group, ]
btestsheowl <- fabssheowl[gbsheowl == testing.group, ]
btestyelrai <- fabsyelrai[gbyelrai == testing.group, ]

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
bc.model.henspa <- bioclim(x= crpdpred, p=ptrainhenspa)
bc.model.horgre <- bioclim(x= crpdpred, p=ptrainhorgre)
bc.model.kenwar <- bioclim(x= crpdpred, p=ptrainkenwar)
bc.model.leabit <- bioclim(x= crpdpred, p=ptrainleabit)
bc.model.logshr <- bioclim(x= crpdpred, p=ptrainlogshr)
bc.model.pibgre <- bioclim(x= crpdpred, p=ptrainpibgre)
bc.model.rusbla <- bioclim(x= crpdpred, p=ptrainrusbla)
bc.model.sheowl <- bioclim(x= crpdpred, p=ptrainsheowl)
bc.model.yelrai <- bioclim(x= crpdpred, p=ptrainyelrai)

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
pred.presence.henspa <- dismo::predict(object= bc.model.henspa, x= crpdpred, ext= e)
pred.presence.horgre <- dismo::predict(object= bc.model.horgre, x= crpdpred, ext= e)
pred.presence.kenwar <- dismo::predict(object= bc.model.kenwar, x= crpdpred, ext= e)
pred.presence.leabit <- dismo::predict(object= bc.model.leabit, x= crpdpred, ext= e)
pred.presence.logshr <- dismo::predict(object= bc.model.logshr, x= crpdpred, ext= e)
pred.presence.pibgre <- dismo::predict(object= bc.model.pibgre, x= crpdpred, ext= e)
pred.presence.rusbla <- dismo::predict(object= bc.model.rusbla, x= crpdpred, ext= e)
pred.presence.sheowl <- dismo::predict(object= bc.model.sheowl, x= crpdpred, ext= e)
pred.presence.yelrai <- dismo::predict(object= bc.model.yelrai, x= crpdpred, ext= e)


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
bc.eval.henspa <- evaluate(p = ptesthenspa, a = btesthenspa, model = bc.model.henspa, x = crpdpred)
bc.eval.horgre <- evaluate(p = ptesthorgre, a = btesthorgre, model = bc.model.horgre, x = crpdpred)
bc.eval.kenwar <- evaluate(p = ptestkenwar, a = btestkenwar, model = bc.model.kenwar, x = crpdpred)
bc.eval.leabit <- evaluate(p = ptestleabit, a = btestleabit, model = bc.model.leabit, x = crpdpred)
bc.eval.logshr <- evaluate(p = ptestlogshr, a = btestlogshr, model = bc.model.logshr, x = crpdpred)
bc.eval.pibgre <- evaluate(p = ptestpibgre, a = btestpibgre, model = bc.model.pibgre, x = crpdpred)
bc.eval.rusbla <- evaluate(p = ptestrusbla, a = btestrusbla, model = bc.model.rusbla, x = crpdpred)
bc.eval.sheowl <- evaluate(p = ptestsheowl, a = btestsheowl, model = bc.model.sheowl, x = crpdpred)
bc.eval.yelrai <- evaluate(p = ptestyelrai, a = btestyelrai, model = bc.model.yelrai, x = crpdpred)

##Determine minimum threshold for "presence"
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
bc.thresh.henspa <- threshold(x= bc.eval.henspa, stat = "spec_sens")
bc.thresh.horgre <- threshold(x= bc.eval.horgre, stat = "spec_sens")
bc.thresh.kenwar <- threshold(x= bc.eval.kenwar, stat = "spec_sens")
bc.thresh.leabit <- threshold(x= bc.eval.leabit, stat = "spec_sens")
bc.thresh.logshr <- threshold(x= bc.eval.logshr, stat = "spec_sens")
bc.thresh.pibgre <- threshold(x= bc.eval.pibgre, stat = "spec_sens")
bc.thresh.rusbla <- threshold(x= bc.eval.rusbla, stat = "spec_sens")
bc.thresh.sheowl <- threshold(x= bc.eval.sheowl, stat = "spec_sens")
bc.thresh.yelrai <- threshold(x= bc.eval.yelrai, stat = "spec_sens")

#function to plot according to what areas
#meet threshold for suitable habitat
plot.thresh.pred <- function(unitibble, pred.presence, bc.thresh) {
  #build custom title for each map
  name <- c(unitibble$common_name[1],
            "threshold predicted range",
            "where model achieves max sum(TPR+TNR)")
  
  #plot base map
  raster::plot(st_geometry(MI),
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
 
  raster::plot(st_geometry(MI),
               add=TRUE,
               boder="grey5")
  box()
}

#using threshold plot function
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
plot.thresh.pred(unihenspa, pred.presence.henspa, bc.thresh.henspa)
plot.thresh.pred(unihorgre, pred.presence.horgre, bc.thresh.horgre)
plot.thresh.pred(unikenwar, pred.presence.kenwar, bc.thresh.kenwar)
plot.thresh.pred(unileabit, pred.presence.leabit, bc.thresh.leabit)
plot.thresh.pred(unilogshr, pred.presence.logshr, bc.thresh.logshr)
plot.thresh.pred(unipibgre, pred.presence.pibgre, bc.thresh.pibgre)
plot.thresh.pred(unirusbla, pred.presence.rusbla, bc.thresh.rusbla)
plot.thresh.pred(unisheowl, pred.presence.sheowl, bc.thresh.sheowl)
plot.thresh.pred(uniyelrai, pred.presence.yelrai, bc.thresh.yelrai)

##plotting Stacked Species Distribution Models

#creating overlap sum file
overlaps <- ((pred.presence.brnthr > bc.thresh.brnthr) + 
               (pred.presence.canwar>bc.thresh.canwar) +
               (pred.presence.comter>bc.thresh.comter) +
               (pred.presence.fiespa>bc.thresh.fiespa) +
               (pred.presence.graspa>bc.thresh.graspa) +
               (pred.presence.marwre>bc.thresh.marwre) +
               (pred.presence.perfal>bc.thresh.perfal) +
               (pred.presence.rehwoo>bc.thresh.rehwoo) +
               (pred.presence.wilfly>bc.thresh.wilfly) +
               (pred.presence.woothr>bc.thresh.woothr) +
               (pred.presence.henspa>bc.thresh.henspa) +
               (pred.presence.horgre>bc.thresh.horgre) +
               (pred.presence.kenwar>bc.thresh.kenwar) +
               (pred.presence.leabit>bc.thresh.leabit) +
               (pred.presence.logshr>bc.thresh.logshr) +
               (pred.presence.pibgre>bc.thresh.pibgre) +
               (pred.presence.rusbla>bc.thresh.rusbla) +
               (pred.presence.sheowl>bc.thresh.sheowl) +
               (pred.presence.yelrai>bc.thresh.yelrai))

# creating color ramp for SSDM map
col <- turbo(n=16)

#plot base map
raster::plot(st_geometry(MI),
             xlim= c(min.lon, max.lon),
             ylim= c(min.lat, max.lat),
             axes= T,
             col='grey95',
             main = "Stacked Species Distribution Models\n 19 birds of conservation concern")

#only plot areas where probability of occurrence
#is greater than the threshold.
raster::plot(overlaps,
             add = TRUE,
             col = col,
             legend= T)

#plot base map
  raster::plot(st_geometry(MI),
               xlim= c(min.lon, max.lon),
               ylim= c(min.lat, max.lat),
               axes= T,
               col='grey95',
               main = "Predicted Habitat Overlap\n for more than 15 species")
  
  #only plot areas where probability of occurrence
  #is greater than the threshold.
  raster::plot((overlaps > 15),
               add = TRUE,
               col = c("grey95", "olivedrab"),
               legend = F)
 
  
  










 








