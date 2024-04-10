
library(tidyverse)
library(terra)
shpdir <- "data/input/shapefiles_phoradendron/"
filList <- list.files(path = shpdir, pattern = ".shp$", full.names = TRUE)
phor_names <- list.files(path = shpdir, pattern = ".shp$") %>% 
  str_remove("_mcp_clip.shp")

shp <- filList %>% purrr::map(terra::vect)
areaList <- 
  shp %>%
  purrr::map_df( function(x) {
    tibble(species = x$name, area =  x$area)
  }) 

