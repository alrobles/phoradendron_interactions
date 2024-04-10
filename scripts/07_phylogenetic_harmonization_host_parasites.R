phoradendron_host_harmonized <- readr::read_csv("data/02_phoradendron_host_harmonized.csv")

phoradendron_host_harmonized
hostTree <-  ape::read.tree("data/05_hostTree.tre")
parasiteTree <- ape::read.tree("data/06_parasiteTree.tre")
hostGenus_phy <- hostTree$tip.label %>% enframe(value = "hostSpecies") %>% 
  mutate(hostGenus = str_extract(hostSpecies, "[^_]+") )

parasiteSpecies_phy <- parasiteTree$tip.label %>% enframe(value = "parasiteSpecies") %>% 
  mutate(parasiteSpecies = str_replace(parasiteSpecies, "_", " ") )



phoradendron_host_phy_harmonized <- phoradendron_host_harmonized %>% 
  dplyr::filter(hostGenus %in% hostGenus_phy$hostGenus) %>% 
  filter(parasiteSpecies %in% parasiteSpecies_phy$parasiteSpecies)
write_csv(phoradendron_host_phy_harmonized, "data/07_phoradendron_host_phy_harmonized.csv")
