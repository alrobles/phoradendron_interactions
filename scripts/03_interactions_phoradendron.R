library(tidyverse)
phoradendron_host_taxonomy <- read_csv("data/02_phoradendron_host_harmonized.csv")


Interactions_Phoradendron_df <- phoradendron_host_taxonomy %>% 
  select(parasiteSpecies, hostGenus) %>% 
  na.exclude() %>% 
  distinct()  
Interactions_Phoradendron_df %>% 
  rename(species = parasiteSpecies, genus = hostGenus ) %>% 
  write_csv("data/03_interactions_phoradendron.csv")
  