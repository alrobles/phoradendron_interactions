library(tidyverse)
host_sample_df <- read_csv("data/07_phoradendron_host_phy_harmonized.csv")

host_sample_taxonomy_df <- host_sample_df %>%
   select(hostOrder, hostFamily, hostGenus)
host_order_count <- host_sample_taxonomy_df %>% 
  distinct() %>% 
  group_by(hostOrder) %>% 
  count(sort = TRUE)
host_order_count_top10 <-  host_order_count %>% head(15)
host_order_count_top10 <- host_order_count_top10 %>% mutate(x = hostOrder, y = n)  
f1_hostOrder <- host_order_count_top10 %>% 
  ggplot( aes(x = reorder(x, -y), y = y)) +
  geom_segment(aes(x = reorder(x, y),
                   xend = reorder(x, y),
                   y = 0, yend = y),
               color = "gray", lwd = 1) +
  geom_point(size = 8, pch = 16, bg = "pink", col = "#f68060", alpha=.6) +
  xlab("Order") +
  ylab("Frequency") +
  coord_flip() +
  theme_bw (base_size = 22)+
  geom_text(aes(label = y), color = "black", size = 4)+theme(axis.text = element_text(size=13))

png("data/08_fig1_parasiteFrequency.png", 480*2, 480*2) 
f1_hostOrder
dev.off()
library(netbiov)
