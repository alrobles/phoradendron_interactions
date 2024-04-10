library(terra)
library(tidyverse)
shp_dir <- "data/input/Shapes_files_america/"
worldPCA <- terra::rast("data/input/PCAraster/WorldPCARast.tiff")

hostSfFiles <- list.files(shp_dir, ".shp$", full.names = TRUE)
hostNames <- hostSfFiles %>%
  str_remove(shp_dir) %>% 
  str_remove(".shp")
America <- rnaturalearth::ne_countries(continent = c("North America", "South America"), returnclass = "sf" )
America <- terra::vect(America)
americaPCA <- terra::crop(worldPCA, America, mask = TRUE)
PCAminmax <- terra::minmax(americaPCA)
x1 <- round(PCAminmax[1])
x2 <- ceiling(PCAminmax[2])
y1 <- round(PCAminmax[3])
y2 <- ceiling(PCAminmax[4])
x <- seq(x1, x2, 0.1)
y <- seq(y1, y2, 0.1)

env_grid <- tidyr::expand_grid(x, y)

get_env_PAM  <- function(x){
  hostPoints <- terra::vect(hostSfFiles[[x]])
  spEnvDf <- terra::extract(americaPCA, hostPoints )
  spEnvDf <- as.matrix(na.exclude(spEnvDf[ ,c(2:3)]))
  spDf <- as.data.frame(hostPoints) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(longitude, latitude) %>% 
    as.matrix()
  r <- terra::rast(env_grid)
  z <- terra::rasterize(spEnvDf, r, value=1, fun=mean)
  M <- as.data.frame(z, xy = TRUE) %>% as_tibble()
  M$host<- hostNames[x]
  return(M)
}
get_env_PAM_safe <- possibly(get_env_PAM, tibble(longitude = NA_real_, latitude = NA_real_, host = NA_character_))


hostList <-   1:length(hostSfFiles) %>% 
  purrr::map(function(x) get_env_PAM_safe(x), .progress = TRUE)

hostDf <- hostList %>% reduce(bind_rows) %>% select(x, y, mean, host)
envPAM <- hostDf %>% distinct() %>% spread(host, mean, fill = 0)
envPAM <- envPAM %>% select(x, y, any_of(hostNames))

write_csv(envPAM, "data/14_envPAM.csv")
