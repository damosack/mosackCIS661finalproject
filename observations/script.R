library(tidyverse)
library(auk)
library(raster)
library(rgdal)
library(dismo)
library(rJava)
library(dplyr)



seloc <- function(tibble) {
  tibble %>% 
    select(longitude, latitude, common_name, county) 
}

brnthr <- read_ebd("brnthr.txt") %>% seloc()
canwar <- read_ebd("canwar.txt") %>% seloc()
comter <- read_ebd("comter.txt") %>% seloc()
fiespa <- read_ebd("fiespa.txt") %>% seloc()
graspa <- read_ebd("graspa.txt") %>% seloc()
marwre <- read_ebd("marwre.txt") %>% seloc()
perfal <- read_ebd("perfal.txt") %>% seloc()
rehwoo <- read_ebd("rehwoo.txt") %>% seloc()
wilfly <- read_ebd("wilfly.txt") %>% seloc()
woothr <- read_ebd("woothr.txt") %>% seloc()

dupes <- function(tibble) {
  dupes2 <- duplicated(tibble[, c('longitude', 'latitude')])
  nodupes <- tibble[!dupes2, ]
  

  return(nodupes)
}

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

