phoradendron_host_taxonomy <- readr::read_csv("data/02_phoradendron_host_harmonized.csv")


options(ENTREZ_KEY = "f6a0ede438f260743f34083fcd46c2efe808")

# get all species for each genus from ncbi
families_species <- unique(phoradendron_host_taxonomy$hostGenus) %>% 
  purrr::map(function(x){
    Sys.sleep(0.11)
    taxize::downstream(x,
                       downto = "Species",
                       db = c("ncbi"), row = 1)
  })

# get a sample of one species for each genus  
families_species_sample <- families_species %>% purrr::map(function(x){
  if(nrow(x[[1]]) == 0 ){
    x[[1]] %>% slice(1)
  } else {
    x[[1]] %>%
      mutate(w = str_count(childtaxa_name, "\\S+")) %>% 
      filter(w == 2) %>%  # parse only binomial taxonomical names
      slice(1)
  }
})

# pair host genus with sampled species
families_species_sample %>% 
  purrr::reduce(bind_rows) %>% as_tibble()

phoradendron_host_species <- tibble(families_species_sample,
       hostGenus = unique(phoradendron_host_taxonomy$hostGenus)    ) %>% 
  unnest(cols = c(families_species_sample)) %>% 
  rename(hostSpecies = childtaxa_name)

phoradendron_host_genus_df <- phoradendron_host_species %>% 
  left_join(phoradendron_host_taxonomy) %>% 
  select(hostOrder, hostFamily, hostGenus, hostSpecies, parasiteOrder, parasiteFamily, parasiteGenus, parasiteSpecies)

write_csv(phoradendron_host_genus_df, "data/04_phoradendron_host_sample_df.csv")

