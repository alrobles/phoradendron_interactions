library(taxize)
library(tidyverse)
phoradendron_host <- read_csv(file = "data/input/Interactions_Phoradendron.csv") %>% 
  janitor::clean_names()

host_genus <- phoradendron_host %>% 
  select(host_family, host) %>% 
  filter(!grepl("ae$", host)) %>% 
  mutate(genus = gsub("([A-Za-z]+).*", "\\1", host)) 
  
host <- host_genus %>% distinct(genus) %>% pull()


db <- host %>% 
  purrr::map(function(x){
    Sys.sleep(0.12)  # to avoid ban in ENTREZ API. 
                    # To decrease sleep time get an entrez key
                    # taxize::use_entrez()
                    # options(ENTREZ_KEY = "somekey")
    taxize::tax_name(x, get = c("family", "order", "genus"), db = "ncbi", rows = 1)
  })

phoradendron_host_taxonomy <- db %>%
  purrr::reduce(rbind) %>% 
  tibble::as_tibble() %>% 
  rename(host = query)

phoradendron_host_taxonomy <- phoradendron_host %>% 
  select(parasite, host) %>% 
  inner_join(phoradendron_host_taxonomy) %>% 
  select(family, order, genus, parasite)

phoradendron_host_taxonomy <- phoradendron_host_taxonomy %>%
  na.exclude() %>%
  distinct() %>% 
  write_csv("data/01_phoradendron_host_taxonomy.csv")


