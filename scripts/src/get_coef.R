get_coef <- function(lm_model){
  stats_log_out <- stats::coef(summary(lm_model))
  conf_int <- stats::confint.default(lm_model)
  lrcoeffs <- data.frame(lm_model$coefficients[1],
                         stats_log_out[1, 2],
                         stats_log_out[1, 3],
                         stats_log_out[1, 4],
                         conf_int[1, 1],
                         conf_int[1, 2],
                         lm_model$coefficients[2],
                         stats_log_out[2, 2],
                         stats_log_out[2, 3],
                         stats_log_out[2, 4],
                         conf_int[2, 1],
                         conf_int[2, 2])
  colnames(lrcoeffs) <- c("intercept", "intercept_std_error",
                          "intercept_z_value", 
                          "intercept_Pr(>|z|)",
                          "intercept_2.5",
                          "intercept_97.5",
                          "slope", "slope_std_error", 
                          "slope_z_value",
                          "slope_Pr(>|z|)", "slope_2.5",
                          "slope_97.5")
  rownames(lrcoeffs) <- NULL 
  return(lrcoeffs)
}
