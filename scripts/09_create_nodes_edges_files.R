library(tidyverse)

hostParasite <- read_csv("data/07_phoradendron_host_phy_harmonized.csv")
edges <- hostParasite %>% 
  select(hostGenus, parasiteSpecies) %>% 
  rename(host = hostGenus, parasite = parasiteSpecies)

nodes <- edges %>% 
  mutate(class = row_number()) %>% 
  gather(class, id) %>% 
  distinct() %>% 
  select(id, class)

write_csv(nodes, "data/09_nodes_phoradendron_host.csv")
write_csv(edges, "data/09_edges_phoradendron_host.csv")

bidajacency_df <- edges %>%
  mutate(value = 1) %>% 
  spread(host, value, 0)
bidajacency_matrix <- bidajacency_df %>% dplyr::select(-parasite) %>% as.matrix()
rownames(bidajacency_matrix) <- bidajacency_df$parasite 
write.csv(bidajacency_matrix, "data/09_biadjacency_matrix.csv")
