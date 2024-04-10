# probability phylogenetic distance ###
PD    <- seq(0, 650, 1) #x axis values across range of phylogenetic distances 
ps1   <- apply(coef_all [ ,c(1,7)], 1, function(x) prob_logit(x, log10(PD+1) ) ) %>% as.data.frame()
colnames(ps1) <-  rownames(i)

colnames(ps1) <-  rownames(i)
ps_all   <- prob_logit(coef[c(1,7)], log10( PD + 1) )


matplot(ps1, type="l",
        ylab="Probability of sharing a Phoradendron parasite",
        xlab="Phylogenetic distance from source to target host (My)",
        cex.lab = 0.5,
        cex=0.5, axes = F,
        col="gray",lwd=0.1,lty=1 )

lines(ps1$`P_quad`, col = yellow, lwd=3, lty = 1)
lines(ps1$`P_cras`, col = sky_blue, lwd=3, lty = 1 )
lines(ps_all, col = vermillion, lwd=2, lty =2 )

legend(300, 1,
       legend = c("Cada muerdago independiente",
                  "", 
                  "P. quadrangulare", 
                  "",
                  "P. crassifolium",
                  "",
                  "Todos los muerdagos"),
       lty= c(1,0,1,0,1,0,1,0,2), col = c("grey", "", yellow,"", bluish_green,
                                          "",sky_blue,"", vermillion ),
       lwd= c(2,0,3,0,3,0,3,0,2), cex = 0.3, bty="n")


axis(1, seq(0, 650, 50), seq(0, 650, 50),
     col.axis= "black", las=1, cex.axis=0.5)
axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1),
     col.axis= "black", las=1, cex.axis=0.5)