boostrap_lm_models <- function (incidence, phydist, geodist, envdist, n)


  {

  repList <-   purrr::map(1:n,
                          function(x){
                            get_lm_models(incidence, phydist, geodist, envdist)
                          }, .progress = TRUE)
  return(repList)
}
