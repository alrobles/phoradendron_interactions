library(tidyverse)
library("V.PhyloMaker2")
host_sample_df <- read_csv("data/04_phoradendron_host_sample_df.csv")
host_example <- host_sample_df %>% select(hostSpecies, hostGenus, hostFamily) 
host_result <- V.PhyloMaker2::phylo.maker(host_example, scenarios=c("S2"))
hostTree <- host_result$scenario.2
hostTree <- multi2di(hostTree)
ape::write.tree(hostTree, "data/05_hostTree.tre")
host_phy_distance_matrix <- hostTree %>% 
  ape::cophenetic.phylo() %>% as.data.frame()
write_csv(host_phy_distance_matrix, "data/05_host_phy_distance_matrix.csv")
