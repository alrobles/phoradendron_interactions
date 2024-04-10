library(tidyverse)
modularity_mean_df <-   read_csv("data/12_max_modularity_df.csv")
parasite_coefficients_mean <- read_csv("data/16_parasite_coefficients_phy.csv")

parasite_coefficients_mean <- parasite_coefficients_mean %>% 
  mutate(parasite = str_replace(parasite, "_",  " "))

parasites_modularity_coefficients <- 
  modularity_mean_df %>% 
  rename(parasite = name) %>% 
  inner_join(parasite_coefficients_mean) 

df <- parasites_modularity_coefficients %>%
  mutate(mu = -intercept/slope) %>%
  mutate(s = 1/slope) %>%
  mutate(connectivity = round(connectivity, 4)) 
View(df)
library(phytools)
parasiteTree <- ape::read.tree("data/06_parasiteTree.tre")

parasiteNames <- df$parasite %>% enframe() %>% 
  mutate(tiplabel = str_replace(value, " ", "_"))

parasiteNamesMatch <- 
  parasiteTree$tip.label %>% enframe(value = "tiplabel") %>% 
  filter(tiplabel %in% parasiteNames$tiplabel)
library(ape)
parasiteTreeMatch <- keep.tip(parasiteTree, tip = parasiteNamesMatch$tiplabel)

parasiteTreeMatch$tip.label


# Modified from https://yulab-smu.top/treedata-book/chapter4.html#color-tree
## Load anole tree
library(phytools)
#anole.tree <- read.tree("http://www.phytools.org/eqg2015/data/anole.tre")

parasiteTreeMatchNames <- parasiteTreeMatch$tip.label %>%
  enframe(name = "tree_id", value = "tiplabel")

parasites_modularity_phy_df <- parasites_modularity_coefficients %>% 
  select(parasite, connectivity) %>% 
  mutate(tiplabel = str_replace(parasite, " ", "_")) %>% 
  left_join(parasiteTreeMatchNames, .) %>% 
  mutate(label = word(parasite, start = 2,end =  2)) %>% 
  mutate(label = paste("P", label))

connectivity_val <- parasites_modularity_phy_df %>% 
  #mutate(connectivity = set_names(tiplabel, connectivity)) %>% 
  pull(connectivity)
names(connectivity_val) <- parasites_modularity_phy_df$label

parasiteTreeMatch$tip.label <- parasites_modularity_phy_df$label
# # Plot with default color scheme
contmap_obj <- contMap(parasiteTreeMatch, connectivity_val, plot = FALSE)

plot(
  contmap_obj,
  type="fan",
  legend = 0.7*max(nodeHeights(parasiteTreeMatch)),
  fsize = c(0.5, 0.7))

# Fit an ancestral state character reconstruction

fit <- phytools::fastAnc(parasiteTreeMatch, connectivity_val, vars = TRUE, CI = TRUE)
fit
# Make a dataframe with trait values at the tips
td <- data.frame(
  node = nodeid(parasiteTreeMatch, names(connectivity_val)),
  trait = connectivity_val)

# Make a dataframe with estimated trait values at the nodes
nd <- data.frame(node = names(fit$ace), trait = fit$ace)

# Combine these with the tree data for plotting with ggtree
d <- rbind(td, nd)
d$node <- as.numeric(d$node)
tree <- dplyr::full_join(parasiteTreeMatch, d, by = 'node')

library(ggtree)
library("scico")
library(tidyverse)
#pdf("data/output/plots/parasite_tree_connectivity.pdf", 21, 21)
png("data/22_parasite_tree_connectivity.png", 960, 960)
ggtree(
  tree, 
  aes(color = trait),
  layout = 'circular',
  ladderize = FALSE, continuous = "color", size = 1)  +
  scale_color_scico(palette = "davos", begin = 1, end = 0) +
  geom_tiplab(hjust = -.1, size = 4, color = "black")

dev.off()
# ggtree(tree, 
#        aes(color = trait), branch.length = "trait"
# )
png("data/22_parasite_tree_connectivity.png", height = 800)
ggtree(
  tree,  mrsd="2013-01-01",
  aes(color = trait), 
  layout = 'circular', 
  ladderize = FALSE, continuous = "color", size = 1.5) +
  # >>> The important part! <<<
  # Choose your favorite scale_color_* function here: 
  scale_color_scico(palette = "bilbao", begin = 1, end = 0) + 
  geom_tiplab(hjust = -.1, size = 6, color = "black") + 
  theme(
    legend.position = c(-0.1, .82),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) 
dev.off()

