#modularidad#
library(rnetcarto)
library(igraph)
library(tidyverse)


nodes <- readr::read_csv("data/09_nodes_phoradendron_host.csv") 
links <- readr::read_csv("data/09_edges_phoradendron_host.csv") %>% 
  rename(TO = host, FROM = parasite) %>% 
  select(FROM, TO)

net <- igraph::graph.data.frame(d = links,
                        vertices = nodes, directed = TRUE)

A <- igraph::get.adjacency(net, sparse=FALSE)
modularity_list <- 1:50 %>% purrr::map(function(x){
  netcarto(A)
}, .progress = TRUE )
readr::write_rds(modularity_list, "data/12_modularity_list.rds")


modularity_value_df <-  
  purrr::map2_df(.x = modularity_list, .y = 1:length(modularity_list),
                 .f = function(x, y){
                   x[[2]] %>% enframe() %>%  mutate(model = y)
                 })  
max_modularity_model <- modularity_value_df %>% arrange(desc(value)) %>% slice(1) %>% pull(model)

modularity_df <-  
  purrr::map2_df(.x = modularity_list, .y = 1:length(modularity_list),
                 .f = function(x, y){
                   x[[1]] %>% mutate(model = y) %>% as_tibble()
                 })  
max_modularity_df <- modularity_df %>% filter(model == max_modularity_model)

nodes <- nodes %>% 
  rename(name = id)

max_modularity_df <- max_modularity_df %>% 
  left_join(nodes) %>% 
  mutate(role = as.factor(role))

readr::write_csv(max_modularity_df, "data/12_max_modularity_df.csv")

#FIG. 4###

library(ggplot2)
library(ggExtra)

###color pallete
grey <- rgb(red= 187,green= 187,blue= 187, max = 255)
red <- rgb(red= 204,green= 51,blue= 17, max = 255)
teal <- rgb(red= 0,green= 153,blue= 136, max = 255)
magenta <- rgb(red= 238,green= 51,blue= 119, max = 255)
cyan <- rgb(red= 51,green= 87,blue= 238, max = 255)
orange <-rgb(red= 238,green= 119,blue= 51, max = 255)

#

plotAll <- max_modularity_df %>% 
  ggplot(aes(x=participation, y=connectivity, group=role))+
  geom_point(aes(color=role),  size = 4, alpha = 0.8)+ 
  scale_color_manual(values=c(grey, cyan, magenta, teal))+
  guides(colour=guide_legend(title = "Rol"))+
  labs(x="Participation (among modules)", y="Connectivity (within modules)")+
  scale_y_continuous(limits = c(-1, 9))+
  scale_x_continuous(limits = c(0, 0.85))+
  theme_bw() + 
  ggtitle("All nodes")

##PARASITOS
modularity_parasite_df <- max_modularity_df %>% 
  filter(class == "parasite")

plotparasitos <- modularity_parasite_df %>% 
 ggplot(aes(x=participation, y=connectivity, group=role))+
   geom_point(aes(color=role), size = 3.5,  alpha = 0.8)+ 
   scale_color_manual(values=c(grey, cyan, magenta, teal))+
   guides(colour=guide_legend(title = "Rol"))+
   labs(x="Participation (among modules)", y="Connectivity (within modules)")+
   scale_y_continuous(limits = c(-1, 9))+
   scale_x_continuous(limits = c(0, .85))+
   theme_bw() +
   ggtitle("Parasites")

 
#HOSPEDEROS
modularity_host_df <- max_modularity_df %>% 
  filter(class == "host")


plothospederos <- modularity_host_df %>%
  ggplot(aes(x=participation, y=connectivity, group=role))+
  geom_point(aes(color=role), size=3.5,alpha = 0.8)+
  scale_color_manual(values=c(grey, cyan, magenta, teal))+
  guides(colour=guide_legend(title = "Role"))+
  labs(x="Participation (among modules)", y="Connectivity (within modules)")+
  scale_y_continuous(limits = c(-1, 9))+
  scale_x_continuous(limits = c(0, 0.85))+
  theme_bw() +
  ggtitle("Host")


png("data/12_plotParasiteParticipation.png", 480, 480)
plotparasitos
dev.off()

png("data/12_12_plotHostParticipation.png", 480, 480)
plothospederos
dev.off()

png("data/12_plotAllParticipation.png", 480, 480)
plotAll
dev.off()

library(cowplot)
png("data/12_plot_hospederos_parasitos_participation_role.png", 480*3, 360)
cowplot::plot_grid(plotAll, plothospederos, plotparasitos, ncol = 3)
dev.off()
