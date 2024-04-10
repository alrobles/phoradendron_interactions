get_mod_env_plot <- function(model_list){
  n <- length(model_list)
  df_from_plot <- purrr::map_df(1:n,  function(index){
    p1 <- ggiraphExtra::ggPredict(model_list[[index]]$mod_env,
                                  interactive = FALSE, colorn = 100, jitter = FALSE)
    data_from_scatterplot <- ggplot2::ggplot_build(p1)
    df_scatterplot <- data_from_scatterplot$data[[1]] %>%
      tibble::as_tibble()
    df_scatterplot %>%
      dplyr::select(x, y) %>%
      dplyr::mutate(model = index)
  })
  p_env <-   df_from_plot %>%
    ggplot() +
    geom_point(aes((x), y), alpha = 0.05) +
    geom_smooth(aes((x), y)) +
    geom_line(aes((x), y, group = model), alpha = 0.05) +
    xlab("Environmental dissimilarity") +
    ylab("Probability of interaction") +
    ylim(c(0, 1))
  return(p_env)
}

