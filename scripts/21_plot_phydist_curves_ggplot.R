library(tidyverse)
parasite_coefficients <- read_csv("data/parasite_coefficients.csv")

# color code
orange <- rgb(red= 230,green= 159,blue= 0, max = 255)
sky_blue <- rgb(	86,180,233, max = 255)
bluish_green	<- rgb(0,158,115, max = 255)
vermillion <- rgb(213,94,0, max = 255)
reddish_purple <- rgb(204,121,167, max = 255)
yellow <-  rgb (240,228,66, max = 255)
white <-  rgb(255,255,255, max = 255 )


### plot
PD    <- seq(0, 750, 1) #x axis values across range of phylogenetic distances 
PD_dissim <- log(PD + 1)/max(log(PD + 1))
ps1   <- apply(parasite_coefficients[ ,c(2,3)], 1, function(x){
  geotax::prob_logit(x, PD_dissim )
}) %>% as.data.frame()

colnames(ps1) <-  parasite_coefficients$parasite



curveValues <- ps1 %>% 
  gather(parasite, value) %>% 
  mutate(x = rep(x = PD_dissim, ncol(ps1), ))

curveValuesFilt <- curveValues %>% 
  filter(parasite %in% c("Phoradendron_quadrangulare",
                         "Phoradendron_crassifolium",
                         "all"))

curveValuesQuadrangulare <- curveValues %>% 
  filter(parasite == c("Phoradendron_quadrangulare"))
curveValuesCrassifolium <- curveValues %>% 
  filter(parasite == c("Phoradendron_crassifolium"))
curveValuesAll <- curveValues %>% 
  filter(parasite == c("all"))


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
pdf("data/output/plots/figure_phoradendron_curves.pdf", width = 7, height = 5)
plot_phoradendron_all
dev.off()
