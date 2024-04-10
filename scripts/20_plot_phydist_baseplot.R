PD    <- seq(0, 750, 1) #x axis values across range of phylogenetic distances 
ps1   <- apply(coef_all_t[ ,c(1,7)], 1, function(x) prob_logit(x, log10(PD+1) ) ) %>% as.data.frame()
colnames(ps1) <-  coef_all_t$parasite
ps_all   <- prob_logit(coef[c(1,7)], log10( PD + 1) )

orange <- rgb(red= 230,green= 159,blue= 0, max = 255)
sky_blue <- rgb(	86,180,233, max = 255)
bluish_green	<- rgb(0,158,115, max = 255)
vermillion <- rgb(213,94,0, max = 255)
reddish_purple <- rgb(204,121,167, max = 255)
yellow <-  rgb (240,228,66, max = 255)
white <-  rgb(255,255,255, max = 255 )

matplot(ps1, type="l",
        ylab = "Probability of sharing a Phoradendron parasite",
        xlab = "Phylogenetic distance from source to target host (My)",
        cex.lab = 0.5,
        cex = 0.5, axes = F,
        col = "gray",lwd=0.1,lty=1 )

lines(PD, ps1$Phoradendron_crassifolium)
plot(PD, ps1$Phoradendron_quadrangulare)

lines(ps1$Phoradendron_quadrangulare, col = yellow, lwd=3, lty = 1)
lines(ps1$Phoradendron_crassifolium, col = sky_blue, lwd=3, lty = 1 )
lines(ps_all, col = vermillion, lwd=2, lty =2 )
