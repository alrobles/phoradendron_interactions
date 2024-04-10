get_model_phy_plot <- function(model_list, addPoints = FALSE, col = "blue"){
  n <- length(model_list)
  df_from_plot <- purrr::map_df(1:n,  function(index){
    p1 <- ggiraphExtra::ggPredict(model_list[[index]]$mod_phy,
                                  interactive = FALSE, colorn = 100, jitter = FALSE)
    data_from_scatterplot <- ggplot2::ggplot_build(p1)
    df_scatterplot <- data_from_scatterplot$data[[1]] %>%
      tibble::as_tibble()
    df_scatterplot %>%
      dplyr::select(x, y) %>%
      dplyr::mutate(model = index)
  })
  if(addPoints == FALSE){
    p_phy <-   df_from_plot %>%
      ggplot() +
      #phydissim <- log(phydist + 1)/max(log(phydist + 1))
      #geom_point(aes((x), y), alpha = 0.05) +
      # scale_x_continuous(breaks = (c(1, 15, 50, 100, 200, 600)),
      #                    trans = "log") +
      geom_smooth(aes((x), y), col = col) +
      #geom_line(aes((x), y, group = model), alpha = 0.05) +
      xlab("Phylogenetic distance") +
      ylab("Probability of interaction")  
  } else {
    p_phy <- p_phy + 
      geom_point(aes((x), y), alpha = 0.05) +
      geom_line(aes((x), y, group = model), alpha = 0.05) 
    
  }
  return(p_phy)
}
