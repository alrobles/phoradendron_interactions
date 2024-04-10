library(terra)
library(tidyverse)
shp_dir <- "data/input/Shapes_files_america/"
hostSfFiles <- list.files(shp_dir, ".shp$", full.names = TRUE)
hostNames <- hostSfFiles %>%
  str_remove(shp_dir) %>% 
  str_remove(".shp")
America <- rnaturalearth::ne_countries(continent = c("North America", "South America"), returnclass = "sf" )
America <- terra::vect(America)

get_PAM  <- function(x){
  hostPoints <- terra::vect(hostSfFiles[[x]])
  spDf <- as.data.frame(hostPoints) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(longitude, latitude) %>% 
    as.matrix()
  
  r <- terra::rast(America, res = 1)
  z <- terra::rasterize(spDf, r, value=1, fun=mean)
  M <- as.data.frame(z, xy = TRUE) %>% as_tibble()
  M$host<- hostNames[x]
  return(M)
}

get_PAM_safe <- possibly(get_PAM, tibble(longitude = NA_real_, latitude = NA_real_, host = NA_character_))

#library(furrr)
#plan(multisession)

hostList <-   1:length(hostSfFiles) %>% 
  purrr::map(function(x) get_PAM_safe(x), .progress = TRUE)

hostDf <- hostList %>% reduce(bind_rows) %>% select(x, y, mean, host)
geoPAM <- hostDf %>% spread(host, mean, fill = 0)
geoPAM <- geoPAM %>% select(x, y, any_of(hostNames))

write_csv(geoPAM, "data/13_geoPAM.csv")
