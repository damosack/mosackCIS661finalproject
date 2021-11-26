library(tidyverse)
library(raster)
library(dismo)
library(terra)
library(rgdal)
library(dplyr)
library(sf)
library(maptools)



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
  raster::plot(st_geometry(MI),
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
 





