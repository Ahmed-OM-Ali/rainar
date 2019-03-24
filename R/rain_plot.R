#' Function that produces plots
#'
#' @param y Response
#' @param obj Object defined as the output of the model fit function
#' @param rain_data input for data
#'
#' @return A combined plot of the estimates from each model and the actual  #'         data
#' @example rain_plot(dublin$RMM,rain_modelling,dublin_airport_data)
rain_plot = function(y,obj,rain_data){

  # Split object
  linear_model_estimations = obj$`Linear model`$fitted.values
  gamma_glm_estimations = obj$`Gamma GLM`$fitted.values

  # Plot of model estimations
  plot(y,main = "Estimating monthly rainfall", xlab = "Month"
       , ylab = "Rainfall in mm", ylim = c(0,600), xlim = c(1,12))
  points(linear_model_estimations, col = "red")
  points(gamma_glm_estimations, col = "blue")
  axis(1,at = seq(1,12, by = 1))
  legend(10,550,legend = c("Actual","LM","GGLM"), col = c("black","red","blue"),
         cex = 1, lty = 1:2)

}


