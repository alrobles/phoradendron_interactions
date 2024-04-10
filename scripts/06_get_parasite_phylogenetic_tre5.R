library(tidyverse)
library("V.PhyloMaker2")
parasite_df <- read_csv("data/04_phoradendron_host_sample_df.csv")
parasite_example <- parasite_df %>% select(parasiteSpecies, parasiteGenus, parasiteFamily) 
parasite_result <- V.PhyloMaker2::phylo.maker(parasite_example, scenarios=c("S2"))
parasiteTree <- parasite_result$scenario.2
parasiteTree <- multi2di(parasiteTree)
ape::write.tree(parasiteTree, "data/06_parasiteTree.tre")
parasite_phy_distance_matrix <- parasiteTree %>% 
  ape::cophenetic.phylo() %>% as.data.frame()
write_csv(parasite_phy_distance_matrix, "data/06_parasite_phy_distance_matrix.csv")
