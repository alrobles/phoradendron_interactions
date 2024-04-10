library(tidyverse)
max_modularity_df <-   read_csv("data/12_max_modularity_df.csv")
parasite_modularity <- max_modularity_df %>% 
  filter(class == "parasite") %>% 
  rename(parasite = name)

parasite_coefficients_mean <- read_csv("data/16_parasite_coefficients_mean.csv")


parasites_modularity_coefficients <- parasite_modularity %>% 
  inner_join(parasite_coefficients_mean) 

df_parasites_modularity <- parasites_modularity_coefficients %>%
  mutate(mu = -intercept/slope) %>%
  mutate(s = 1/slope) %>%
  mutate(connectivity = round(connectivity, 4)) 

df_parasites_modularity <- df_parasites_modularity %>% 
  filter(connectivity > 0.1)

df_parasites_modularity <- df_parasites_modularity %>% mutate(id = row_number())

train <- df_parasites_modularity %>% sample_frac(.65)

#Create test set
test  <- anti_join(df_parasites_modularity, train, by = 'id')

model_connectivity <- lm(data = train, formula = connectivity ~ mu + s  )
dataTest <- broom::augment(model_connectivity, newdata = test)
broom::glance(model_connectivity, newdata = test)

broom::augment(model_connectivity, newdata = test) %>% 
  select(connectivity, .fitted) %>% cor()

# plot_fitted <- dataTest %>%
#   ggplot(aes(connectivity, .fitted)) + geom_point() +
#   geom_smooth(method = "lm")


library(viridis)
library(tidyverse)

mytheme <- theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
                 legend.title =element_text(size = 30, face = "bold"),
                 legend.text =element_text(size = 25, face = "bold"),
                 axis.text.x = element_text(size = 30, face = "bold"),
                 axis.title.x = element_text(size = 30, face = "bold"),
                 axis.text.y = element_text(size = 30, face = "bold"),
                 axis.title.y = element_text(size = 30, face = "bold"),
                 plot.margin = unit(c(0.3,0.3, 0.3, 0.3), "cm"))

p1 <- df_parasites_modularity %>%
  ggplot(aes(mu, s)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_viridis(discrete = FALSE,
                     guide= "none",
                     option = "F",
                     begin = 1,
                     end = 0,
                     direction = 1,
                     alpha = 0.8) +
  xlim(c(0, 1))+
  ylim(c(-0.3, 0)) +
  geom_point(data = df, aes(mu, s, col = connectivity, size = connectivity))  +
  scale_size(guide="none") +
  scale_colour_gradient(low="lightyellow", high="red") +
  labs(x=expression(mu), y="s")  +
  theme_bw() +
  ggtitle(
    expression(
      paste("Connectivity in ", mu, " - s parameter space") 
    ))+
  mytheme



png("data/17_s_mu_connectivity.png", 480*2, 480*2)
p1
dev.off()

pdf("data/17_s_mu_connectivity.pdf", 11, 11)
p1
dev.off()


p2 <- ggiraphExtra::ggPredict(model_connectivity,
                              interactive = TRUE, 
                              colorn = 25,
                              digits = 2,
                              show.summary = TRUE,
                              jitter = FALSE, se = TRUE) 

p2
model_connectivity$model %>% View()


View(train)
