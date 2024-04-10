library(taxize)
library(tidyverse)

phoradendron_parasite <- read_csv(file = "data/01_phoradendron_host_taxonomy.csv") %>% 
  janitor::clean_names()

parasite_genus <- phoradendron_parasite %>% 
  select(parasite) %>%
  distinct() %>% 
  filter(!grepl("ae$", parasite)) %>% 
  mutate(genus = gsub("([A-Za-z]+).*", "\\1", parasite)) 

parasite_sp <- parasite_genus %>% distinct(parasite) %>% pull()

Phoradentron_gbif_id <- taxize::get_gbifid("Phoradendron")

parasite_gbif <- taxize::gbif_downstream(Phoradentron_gbif_id[[1]], downto = "species")

db_gbif <- parasite_sp %>% 
  purrr::map(function(x){
    Sys.sleep(0.13)  # to avoid ban in ENTREZ API. 
    taxize::gbif_parse(x)
    
  })

db_gbif <- db_gbif %>% 
reduce(bind_rows) %>% 
  mutate(query = parasite_sp) %>% 
  as_tibble()

db_gbif_clean <- db_gbif %>% na.exclude()

db_gbif_clean <- db_gbif_clean %>% 
  select(genusorabove, scientificname) %>% 
  rename(genus = genusorabove,
         species = scientificname) %>% 
  mutate(family = "Viscaceae") %>% 
  mutate(order = "Santalales")
  select(family, genus, species)

phoradendron_parasite_clean <- phoradendron_parasite %>%
  filter(parasite %in% db_gbif_clean$species )

phoradendron_parasite_clean <- phoradendron_parasite_clean %>% 
  rename(hostFamily = family, hostOrder = order, hostGenus = genus) %>% 
  rename(parasiteSpecies = parasite)
db_gbif_clean <- db_gbif_clean %>% 
  rename(parasiteOrder = order, parasiteFamily = family, parasiteGenus = genus, parasiteSpecies = species)

phoradendron_host_harmonized <- full_join(phoradendron_parasite_clean, db_gbif_clean)  

phoradendron_host_harmonized %>% 
  select(hostOrder, hostFamily, hostGenus, parasiteOrder, parasiteFamily, parasiteGenus, parasiteSpecies ) %>% 
  write_csv("data/02_phoradendron_host_harmonized.csv")
