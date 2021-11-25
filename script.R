library(tidyverse)
library(auk)
library(raster)
library(terra)
library(rgdal)
library(dplyr)
library(sf)



#create function to select relevant variables from datasets
seloc <- function(tibble) {
  tibble %>% 
    select(longitude, latitude, common_name, county) 
}

#read in data, select relevant variables, assign to objects
brnthr <- read_ebd("observations/brnthr.txt") %>% seloc()
canwar <- read_ebd("observations/canwar.txt") %>% seloc()
comter <- read_ebd("observations/comter.txt") %>% seloc()
fiespa <- read_ebd("observations/fiespa.txt") %>% seloc()
graspa <- read_ebd("observations/graspa.txt") %>% seloc()
marwre <- read_ebd("observations/marwre.txt") %>% seloc()
perfal <- read_ebd("observations/perfal.txt") %>% seloc()
rehwoo <- read_ebd("observations/rehwoo.txt") %>% seloc()
wilfly <- read_ebd("observations/wilfly.txt") %>% seloc()
woothr <- read_ebd("observations/woothr.txt") %>% seloc()

#create function to remove duplicate observations at same location witin
#each data set.
dupes <- function(tibble) {
  dupes2 <- duplicated(tibble[, c('longitude', 'latitude')])
  nodupes <- tibble[!dupes2, ]
  

  return(nodupes)
}

#assign data sets without duplicates to new objects
unibrnthr <- dupes(brnthr)
unicanwar <- dupes(canwar)
unicomter <- dupes(comter)
unifiespa <- dupes(fiespa)
unigraspa <- dupes(graspa)
unimarwre <- dupes(marwre)
uniperfal <- dupes(perfal)
unirehwoo <- dupes(rehwoo)
uniwilfly <- dupes(wilfly)
uniwoothr <- dupes(woothr)

midwest <- st_read("mi2/Shape/GU_StateOrTerritory.shp")
mi <- midwest[2, ]
plot(st_geometry(mi))

climateallfiles <- as_tibble(list.files(path = "climatetiff" ,
                                        pattern = '.', 
                                        full.names=TRUE))


plot(raster("climatetiff/current_2-5arcmin_annualPET.tif"))

rasters <- raster::stack(climateallfiles)
rasters


