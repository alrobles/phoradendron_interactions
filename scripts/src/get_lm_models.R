get_lm_models <- function (incidence, phydist, geodist, envdist)
{
  if (nrow(incidence) == 1) {
    l <- list(names(incidence[, apply(incidence, 2, function(x) (x ==
                                                                   1))]))
    names(l) <- rownames(incidence)
  }
  else {
    l <- lapply(apply(apply(incidence, 2, function(x) (x ==
                                                         1)), 1, which), names)
  }
  focal <- lapply(l, function(x) sample(x, 1))
  df_phy <- do.call(rbind, lapply(names(focal), function(x) data.frame(colnames(incidence),
                                                                       focal[[x]], phydist[, focal[[x]]], as.numeric(incidence[x,
                                                                       ]), x)))
  colnames(df_phy) <- c("tohost", "fromhost", "phydist", "suscept",
                        "incidence")

  df_geo <- do.call(rbind, lapply(names(focal), function(x) data.frame(colnames(incidence),
                                                                       focal[[x]], geodist[, focal[[x]]], as.numeric(incidence[x,
                                                                       ]), x)))
  colnames(df_geo) <- c("tohost", "fromhost", "geodist", "suscept",
                        "incidence")

  df_env <- do.call(rbind, lapply(names(focal), function(x) data.frame(colnames(incidence),
                                                                       focal[[x]], envdist[, focal[[x]]], as.numeric(incidence[x,
                                                                       ]), x)))
  colnames(df_env) <- c("tohost", "fromhost", "envdist", "suscept",
                        "incidence")
  df <- dplyr::left_join(df_env, df_geo,
                  by = c("tohost", "fromhost","suscept",
                         "incidence")) %>%
    dplyr::left_join(df_phy,
              by = c("tohost", "fromhost","suscept",
                     "incidence"))

  #log_out <- stats::glm(suscept ~ phydist , data = df, family = stats::binomial(link = "logit"))
  #log_out <- stats::glm(suscept ~ phydist + geodist + envdist, data = df, family = stats::binomial(link = "logit"))
  #log_out <- stats::glm(suscept ~ phydist*geodist*envdist, data = df, family = stats::binomial(link = "logit"))
  #mod_phy <- stats::glm(df$suscept ~ df$phydist, family = stats::binomial(link = "logit"))
  mod_phy <- stats::glm(suscept ~ phydist, data = df, family = stats::binomial(link = "logit"))
  mod_geo <- stats::glm(suscept ~ geodist, data = df, family = stats::binomial(link = "logit"))
  mod_env <- stats::glm(suscept ~ envdist, data = df, family = stats::binomial(link = "logit"))
  mod_phy_geo <- stats::glm(suscept ~ phydist + geodist, data = df, family = stats::binomial(link = "logit"))
  mod_phy_env <- stats::glm(suscept ~ phydist + envdist, data = df, family = stats::binomial(link = "logit"))
  mod_geo_env <- stats::glm(suscept ~ geodist + envdist, data = df, family = stats::binomial(link = "logit"))
  mod_phy_geo_env <- stats::glm(suscept ~ phydist + geodist + envdist, data = df, family = stats::binomial(link = "logit"))
  mod_phy_geo_env_int <- stats::glm(suscept ~ phydist*geodist*envdist, data = df, family = stats::binomial(link = "logit"))
  modelList <- list(mod_phy,
                    mod_geo,
                    mod_env,
                    mod_phy_geo,
                    mod_phy_env,
                    mod_geo_env,
                    mod_phy_geo_env,
                    mod_phy_geo_env_int
                    )
  names(modelList) <- c("mod_phy",
                           "mod_geo",
                           "mod_env",
                           "mod_phy_geo",
                           "mod_phy_env",
                           "mod_geo_env",
                           "mod_phy_geo_env",
                           "mod_phy_geo_env_int")
  return(modelList)
}

