#' This is a generic function for fit. It can be called on objects of any class.
#' For objects that are not models or model family's it simply prints a message
#' stating that you are "in fit.default".
#' @param mod The model or model family object. This will likely be empty in the default case.
#' @param hp_space The hyperparameter space. This will likely be empty in the default case.
#' @param hp_strategy The hyperparameter strategy. This will likely be empty in the default case.
#' @param calibration The calibration. This will likely be empty in the default case.
#' @param k The k value. This will likely be empty in the default case.
#' @param metric The metric. This will likely be empty in the default case.
#' @export
#' @examples
#' fit()
fit <- function(mod,hp_space,hp_strategy,calibration, k,metric) UseMethod("fit")
fit.default <- function(mod,k,metric) {
  cat("in fit.default\n")
  invisible(NULL)
}

#' This specifies what to do when an object of class "model" is passed into the fit function.
#' This will call the fitmodel function on the model with it's corresponding parameters to
#' fit the model.
#' @param mod The model to be fit.
#' @param hp_space The hyperparameter space. This is unused for objects of class "model".
#' @param hp_strategy The hyperparameter strategy. This is unused for objects of class "model".
#' @param calibration The calibration. This is unused for objects of class "model".
#' @param k The k value for the model. Can also be any generic parameter value. Default is set
#' to -1 which is calculated to a more sane default when passed into the fitmodel function.
#' @param metric The metric to use for fitting the model. Default is set to "eculidian".
#' @export
#' @return The fitted model for mod. This is of class "model".
#' @examples
#' fit_mod <- fit(mod = new_knn_model(data=iris, design = Species ~ .), k=3, metric="euclidian")
fit.model <- function(mod,hp_space,hp_strategy,calibration, k=-1,metric="euclidian") {
  ret_mod <- fitmodel(mod,k,metric)
  return (ret_mod)
}

#' This specifies what to do when an object of class "modelfamily" is passed into the fit function.
#' This will call the fitmodelfamily function on the modelfamily with it's corresponding parameters to
#' fit the model family.
#' @param mod The model family to be fit.
#' @param hp_space The hyperparameter space. This specifies the hyperparameters in the model
#' family.
#' @param hp_strategy The hyperparameter strategy. This specifies how the hyperparameters
#' are selected when fitting the model family.
#' @param calibration The calibration. This specifies how each model will be assessed when
#' fitting the model family.
#' @param k The k value. This is unused for objects of class "modelfamily".
#' @param metric The metric. This is unused for objects of class "modelfamily".
#' @export
#' @return The fitted modelfamily for mod. This is of class "modelfamily".
#' @examples
#' fitted_model_fam <- fit(mod = new_knn_modelfamily(data = iris,design = Species ~ .),
#' hp_space = hp_space(vals = c(5,3,1), metric = c("euclidian")),
#' hp_strategy = "grid_search",
#' calibration = calibration(score = "accuracy",sampling = "bootstrap",reps = 10))
fit.modelfamily <- function(mod,hp_space,hp_strategy,calibration,k,metric) {
  ret_mod <- fitmodelfamily(mod,hp_space,hp_strategy,calibration)
  return (ret_mod)
}
