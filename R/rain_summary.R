#' Produce summary statistics for the rain data
#'
#' @description rain_summary
#'
#' @param y Response
#' @param X covariate
#' @param obj Input obj that contains models run
#'
#' @return Summary statistics
#'
#' @example rain_modelling = rain_fit(dublin_airport_data$RMM,dublin_airport_data$mtemp, dublin_airport_data), rain_summary(dublin_airport_data$RMM,dublin_airport_data$mtemp,rain_modelling)
rain_summary = function(y,X,obj){

  # Coefficients for each model
  coeff_linear = obj$`Linear model`$coefficients
  coeff_GLM = obj$`Gamma GLM`$coefficients

  # Confidence interval
  CI_linear = confint(obj$`Linear model`)
  CI_GLM = confint(obj$`Gamma GLM`)

  # Produce AIC for each model
  AIC_linear = AIC(obj$`Linear model`)
  AIC_GLM = AIC(obj$`Gamma GLM`)

  # F test for linear model
  anova_linear = anova(obj$`Linear model`)
  Ftest_value = anova_linear$`F value`[1]
  Ftest_pvalue = anova_linear$`Pr(>F)`[1]

  # Deviance
  deviance_lm = deviance(obj$`Linear model`)
  deviance_GLM = deviance(obj$`Gamma GLM`)

  return(list("AIC comparisons" =c("Linear model" = AIC_linear,"Gamma GLM" = AIC_GLM), "Coefficients" = c("Linear model" = coeff_linear,"Gamma GLM" = exp(coeff_GLM)),"Confidence Intervals" = c("Linear model" = CI_linear, "Gamma GLM" = exp(CI_GLM)), "F-test for linear model" = c("F-test value" = Ftest_value, "p value" = Ftest_pvalue), "Deviance" = c("Linear model" = deviance_lm, "Gamma GLM" = deviance_GLM )))

}
