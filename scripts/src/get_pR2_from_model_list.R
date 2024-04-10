get_pR2_from_model_list <- function(model_list){
  n <- length(model_list)
  modelNames <- c("mod_phy"
                  ,"mod_geo"
                  ,"mod_env"
                  ,"mod_phy_geo"
                  ,"mod_phy_env"
                  ,"mod_geo_env"
                  ,"mod_phy_geo_env"
                  ,"mod_phy_geo_env_int")

  pr2List <- purrr::map_df(modelNames, function(model_names){
    purrr::map_df(.x = 1:n,
                  function(x){
                    suppressMessages({
                      pscl::pR2(model_list[[x]][[model_names]]) %>%
                        tibble::enframe() %>%
                        tidyr::spread(name, value)  %>%
                        dplyr::mutate(modelName  = model_names) %>%
                        dplyr::mutate(repetition = x)
                    })
                  })
  })
  return(pr2List)
}
