# library("moonBook")
# library(TH.data)
# library("ggiraphExtra")

library(tidyverse)
source("scripts/src/boostrap_lm_models.R")
source("scripts/src/get_lm_models.R")
source("scripts/src/get_pR2_from_model_list.R")
source("scripts/src/get_coef.R")

biadjacency <- readr::read_csv("data/09_biadjacency_matrix.csv")
filterIndex <- rowSums(biadjacency[, -1]) > 2
biadjacency <- biadjacency[filterIndex, ]
rNames <- biadjacency[ , 1] %>% pull()

geodist <- read_csv("data/15_geoJaccard.csv") %>%
  as.matrix()
rownames(geodist) <- colnames(geodist)

envdist <- read_csv("data/15_envJaccard.csv") %>%
  as.matrix()
rownames(envdist) <- colnames(envdist)
phydist <- read_csv("data/05_host_phy_distance_matrix.csv") %>%
  as.matrix()
colnames(phydist) <- str_extract(colnames(phydist), "[^_]+")
rownames(phydist) <- colnames(phydist)
phyNames <- rownames(phydist) %>%
  enframe(name = "phydist")

geoNames <- rownames(geodist) %>%
  enframe(name = "geodist")
envNames <- rownames(envdist) %>%
  enframe(name = "envdist")

biadjacencyNames <- colnames(biadjacency) %>%
  enframe(name = "biadjacency")
selectedNames <- list(phyNames, geoNames, envNames, biadjacencyNames) %>%
  reduce(inner_join) %>%  pull(value)

phydist <- phydist[selectedNames, selectedNames]
geodist <- geodist[selectedNames, selectedNames]
envdist <- envdist[selectedNames, selectedNames]

biadjacency_filter <- biadjacency[, selectedNames] %>%
  as.matrix(  )

rownames(biadjacency_filter) <- pull(biadjacency[,1 ])
biadjacency_filter %>% colSums()
FilterZeroCols <- which(colSums(biadjacency_filter) !=  0)

phydist <- phydist[names(FilterZeroCols), names(FilterZeroCols)]
geodist <- geodist[names(FilterZeroCols), names(FilterZeroCols)]
envdist <- envdist[names(FilterZeroCols), names(FilterZeroCols)]
biadjacency_filter <- biadjacency_filter[ ,FilterZeroCols]
phydissim <- log(phydist + 1)/max(log(phydist + 1))
modelList <- boostrap_lm_models(biadjacency_filter, phydissim, geodist, envdist, 100)

#readr::write_rds(modelList, "data/16_modelList.rds")


each_parasite_modelList <- purrr::map(1:nrow(biadjacency_filter), function(x){
  boostrap_lm_models(biadjacency_filter[x, , drop=FALSE],
                     phydissim, geodist, envdist, 3)
}, .progress = TRUE)

names(each_parasite_modelList) <- rownames(biadjacency_filter)
#readr::write_rds(each_parasite_modelList, "data/16_each_parasite_modelList.rds")

each_parasite_coefficients <- purrr::map2(each_parasite_modelList, 
                                          names(each_parasite_modelList), 
                                          function(x, y){
                                            x %>% 
                                              purrr::map_df(function(x){
                                                get_coef(lm_model = x$mod_phy)
                                              } ) %>% 
                                              as_tibble() %>% 
                                              mutate(parasite = y)
                                            
                                          }, .progress = TRUE)

each_parasite_coefficients_geo <- purrr::map2(each_parasite_modelList, 
                                              names(each_parasite_modelList), 
                                              function(x, y){
                                                x %>% 
                                                  purrr::map_df(function(x){
                                                    get_coef(lm_model = x$mod_geo)
                                                  } ) %>% 
                                                  as_tibble() %>% 
                                                  mutate(parasite = y)
                                                
                                              }, .progress = TRUE)

parasite_coefficients_mean <- each_parasite_coefficients %>% 
  reduce(bind_rows) %>% 
  select(intercept, slope, parasite) %>% 
  gather(parameter, value, -parasite) %>% 
  group_by(parasite, parameter) %>%
  summarise(value = mean(value)) %>% 
  spread(parameter, value)

#write_csv(parasite_coefficients_mean, "data/16_parasite_coefficients_mean.csv")

phydist_coefficients <- modelList %>% 
  purrr::map_df(function(x){
    get_coef(lm_model = x$mod_phy)
  } ) %>% 
  as_tibble()

geodist_coefficients <- modelList %>% 
  purrr::map_df(function(x){
    get_coef(lm_model = x$mod_geo)
  } ) %>% 
  as_tibble()
env_coefficients <- modelList %>% 
  purrr::map_df(function(x){
    get_coef(lm_model = x$mod_env)
  } ) %>% 
  as_tibble()

parasite_coefficients_all <- parasite_coefficients_mean %>% 
  mutate(parasite = "all") %>% 
  select(intercept, slope, parasite) %>% 
  gather(parameter, value, -parasite) %>% 
  group_by(parasite, parameter) %>%
  summarise(value = mean(value)) %>% 
  spread(parameter, value)

parasite_coefficients <- bind_rows(parasite_coefficients_mean, parasite_coefficients_all)

#write_csv(parasite_coefficients, "data/16_parasite_coefficients.csv")# 

###color pallete

orange <- rgb(red= 230,green= 159,blue= 0, max = 255)
sky_blue <- rgb(	86,180,233, max = 255)
bluish_green	<- rgb(0,158,115, max = 255)
vermillion <- rgb(213,94,0, max = 255)
reddish_purple <- rgb(204,121,167, max = 255)
yellow <-  rgb (240,228,66, max = 255)
white <-  rgb(255,255,255, max = 255 )
# ### plot
PD <- 0:650
PD_dissim <- log(PD + 1)/max(log(PD + 1))
ps1   <- apply(parasite_coefficients[ ,c(2,3)], 1, function(x){
  geotax::prob_logit(x, PD_dissim )
}) %>% as.data.frame()

colnames(ps1) <-  parasite_coefficients$parasite



curveValues <- ps1 %>%
  gather(parasite, value) %>%
  mutate(x = rep(x = PD_dissim, ncol(ps1), ))
# 
curveValuesFilt <- curveValues %>%
  filter(parasite %in% c("Phoradendron_quadrangulare",
                         "Phoradendron_crassifolium",
                         "all"))
# 
curveValuesQuadrangulare <- curveValues %>%
  filter(parasite == c("Phoradendron_quadrangulare"))
curveValuesCrassifolium <- curveValues %>%
  filter(parasite == c("Phoradendron_crassifolium"))
curveValuesAll <- curveValues %>%
  filter(parasite == c("all"))
# 
# 
plot_phoradendron_all <- curveValues %>%
  ggplot() + geom_line(aes(x, value, group = parasite),
                       col="gray", alpha =  0.5) +
  geom_line(data = curveValuesQuadrangulare,
            aes(x, value), col = yellow, linewidth = 2, ) +
  geom_line(data = curveValuesCrassifolium,
            aes(x, value), col = sky_blue, linewidth = 2) +
  geom_line(data = curveValuesAll,
            aes(x, value), col = vermillion, linewidth = 2, linetype = 2) +
  geom_line(data = curveValuesFilt, aes(x, value, col = parasite)) +
  scale_color_manual(name="Parasite",
                     #breaks=c('all', 'Ph. crassifolium', 'Ph. quadrangulare'),
                     labels = c('All',
                                expression(italic('Ph. crassifolium')),
                                expression(italic('Ph. quadrangulare'))),
                     values=c(vermillion,
                              sky_blue,
                              yellow)) +
  theme_bw() +
  ylab("Probability of sharing a Phoradendron parasite") +
  xlab("Phylogenetic dissimilarity from source to target host (My)") +
  theme(legend.position = c(0.15, 0.2))
# pdf("data/output/plots/figure_phoradendron_curves.pdf", width = 7, height = 5)
plot_phoradendron_all
# dev.off()
# 
{
  matplot(ps1, type="l",
          ylab="Probability of sharing a Phoradendron parasite",
          xlab="Phylogenetic distance from source to target host (My)",
          cex.lab = 0.5,
          cex=0.5, axes = F,
          col="gray",lwd=0.1,lty=1 )
  
  lines(ps1$Phoradendron_quadrangulare, col = yellow, lwd=3, lty = 1)
  lines(ps1$Phoradendron_crassifolium, col = sky_blue, lwd=3, lty = 1 )
  lines(ps1$all, col = vermillion, lwd=2, lty =2 )
  axis(1, seq(0, 650, 50), seq(0, 650, 50),
       col.axis= "black", las=1, cex.axis=0.5)
  axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1),
       col.axis= "black", las=1, cex.axis=0.5)
}
# 
# 
# 

output_pR2 <- get_pR2_from_model_list(modelList)

p1_boxplot <- output_pR2 %>%
  filter(modelName %in% c("mod_phy", "mod_geo", "mod_env", "mod_phy_geo_env")) %>%
  ggplot() + geom_boxplot(aes(McFadden, modelName )) +
  ylab("Model Name") +
  xlab("Cragg and Uhler's pseudo r-squared")

png("data/16_p1_boxplot_3_models.png", 480, 480)
p1_boxplot
dev.off()

# png("data/16_p_combined_vars.png", 640, 640)
# #pdf("p_combined_vars.pdf")
# p_combined_vars
# dev.off()

source("scripts/src/get_mod_phy_plot.R")
source("scripts/src/get_mod_geo_plot.R")
source("scripts/src/get_mod_env_plot.R")
p_phy <- get_mod_phy_plot(modelList)
p_geo <- get_mod_geo_plot(modelList)
p_env <- get_mod_env_plot(modelList)

library(cowplot)
mytheme <- theme_minimal_grid(
  font_size = 18,
  color = "grey70"
)


p1_boxplot <- p1_boxplot  + mytheme
p_phy <- p_phy + mytheme
p_geo <- p_geo + mytheme
p_env <- p_env + mytheme

png("data/16_LinearModelsPlot_dissimilarity_1.png", 960, 640)
#pdf("LinearModelsPlot_dissimilarity_1.pdf", width = 14, height = 7)
cowplot::plot_grid(p1_boxplot, p_phy, p_geo, p_env) +
  theme_half_open()+
  panel_border() +
  background_grid()
dev.off()

####
