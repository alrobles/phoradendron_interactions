
library(tidyverse)
library(terra)
edges <- read_csv("data/09_edges_phoradendron_host.csv")

cali <- readr::read_csv("data/input/shapefiles_phoradendron/phor_cali_train.csv")
shp_cali <- cali %>% sf::st_as_sf(coords = c("long", "lat")) 
shp_convex <- shp_cali %>% sf::st_convex_hull()  
shp_convex <- sf::st_union(shp_cali) %>% sf::st_convex_hull()
plot(shp_convex)
library(sf)
NorthAmerica_shp <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf")
NorthAmerica_shp <- NorthAmerica_shp %>% 
  select(featurecla)
NorthAmerica_shp
plot(shp_convex)
sf::st_crs(shp_convex) <- sf::st_crs(NorthAmerica_shp)
plot(shp_convex)
shp_convex <- sf::st_intersection(shp_convex,NorthAmerica_shp ) %>% 
  sf::st_union() %>%
  vect() %>% 
  sf::st_as_sf() %>% 
  mutate(name = "Phoradendron californicum") 
  plot()
shp_convex$area <- sf::st_area(shp_convex) 
shp_convex %>% 
  write_sf("data/input/shapefiles_phoradendron/phor_californicum_mcp_clip.shp")


host_range <- edges %>% group_by(parasite) %>% count()

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




library(tidyverse)
library(terra)
shpdir <- "data/input/shapefiles_phoradendron/"
filList <- list.files(path = shpdir, pattern = ".shp$", full.names = TRUE)
phor_names <- list.files(path = shpdir, pattern = ".shp$") %>% 
  str_remove("_mcp_clip.shp")

shp[[11]] %>% plot()

shp <- filList %>% purrr::map(terra::vect)
areaList <- 
  shp %>%
  purrr::map_df( function(x) {
    tibble(species = x$name, area =  x$area)
  }) 


library(tidyverse)
coef_phy <- read_csv("data/16_parasite_coefficients_phy.csv")
coef_geo <- read_csv("data/16_parasite_coefficients_geo.csv")

coef_area <- 
areaList %>% 
  rename(parasite = species) %>% 
  left_join(coef_phy ) 

host_range_coef_area <- inner_join(coef_area, host_range)
  
host_range_coef_area %>%
  ggplot() + geom_point(aes((area), log(n)))
host_range_coef_area %>% select(area ,n) %>% mutate_all(log) %>% cor()

coef_area %>% ggplot() + geom_point(aes(log(area), slope)) + geom_smooth(aes(log(area), slope), method = "lm")


host_range_coef_area %>% 
  filter(n > 3) %>% 
  ggplot() + geom_point(aes(log(area), n)) + geom_text(aes(log(area), n, label = parasite)) + 
  geom_smooth(aes(log(area), n, label = parasite), method = "lm")


ols_regression <- lm(log_area ~ slope, coef_area) 
coef(ols_regression) 
coef_area <- coef_area %>% 
  mutate(log_area = log(area))

theil_sen <- mblm(log_area ~ slope, coef_area) 
summary(theil_sen)
broom::augment(ols_regression) %>% 
  ggplot() + geom_point(aes(.fitted, log_area ))

coef_area %>% 
  ggplot(aes(log_area, slope)) +
  geom_point()
