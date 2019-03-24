#' A Function to fit models on rain data
#'
#' @description rain_fit fits a linear model and a gamma glm to the rainfall
#'              dataset inputed by the user
#'
#' @param y response
#' @param X covariate
#' @param raindata input data
#'
#' @return Summary of fitted models
#'
#' @example rain_modelling = rain_fit(dublin_airport_data$RMM,dublin_airport_data$mtemp, dublin_airport_data)
rain_fit = function(y,X,raindata){

  # Fit linear model
  model_lm = lm(y ~ X - 1, data = raindata)

  # Fit Gamma GLM
  model_gglm = glm(y ~ X - 1, family = Gamma(link = "log"), data = raindata)

  return(list("Linear model" = model_lm, "Gamma GLM" = model_gglm))

}
