#conectancia#
library(bipartite)
edges <- readr::read_csv("data/09_edges_phoradendron_host.csv")
biadjacency_df <- edges %>%
  mutate(value = 1) %>% 
  spread(host, value, 0)
biadjacency_matrix <- biadjacency_df %>% dplyr::select(-parasite) %>% as.matrix()
rownames(biadjacency_matrix) <- biadjacency_df$parasite 
phoradendron_network_level <- networklevel(biadjacency_matrix, index = "binary")

broom::tidy(phoradendron_network_level) %>% 
  rename(value = x) %>% 
  mutate(value = round(value, 2)) %>% 
  write_csv("data/10_phoradendron_network_level_tbl.csv")

#anidamiento#
library(vegan)
library(bipartite)


out_list <- replicate(100, nestedness(biadjacency_matrix))

out_list <- purrr::map(1:100, function(x){
  nestedness(biadjacency_matrix)
}, .progress = TRUE)
readr::write_rds(out_list, "data/11_nestedness_output_list.rds")


phoradentron_temperature <- out_list %>% purrr::map_df(function(x) x$statistic %>% enframe())  

figure_nestedness_boxplot <- phoradentron_temperature %>% 
  rename(Parameter = name ) %>%
  ggplot() + geom_boxplot(aes(Parameter, value, fill = Parameter), alpha = 0.3) +
  theme_bw()
png("data/11_nestedness_temprerature_boxplot.png", 480, 480)
figure_nestedness_boxplot
dev.off()

phoradendron_nest_df <- nestednodf(biadjacency_matrix, 
           order = TRUE, 
           weighted = FALSE, 
           wbinary = TRUE)

capture.output(phoradendron_nest_df) %>% as_tibble() %>% 
  readr::write_csv("data/11_phoradendron_nest_df.csv")
