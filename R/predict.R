#' This is a generic function for predict. It can be called on objects of any class.
#' For objects that are not models or model family's it simply prints a message
#' stating that you are "in predict.default".
#' @param mod The model or model family object. This will likely be empty in the default case.
#' @param newdata The new data you want to predict values for. This will likely be empty in
#' the default case.
#' @export
#' @examples
#' predict()
predict <- function(mod,new_data) UseMethod("predict")
predict.default <- function(mod,new_data) {
  cat("in predict.default\n")
  invisible(NULL)
}

#' This specifies what to do when predict is called on an object of class "model". This will
#' call the function predictmodel on the model with it's corresponding parameters to
#' predict the values for the new_data using the model.
#' @param mod The model object. This model must be fitted.
#' @param newdata The new data you want to predict values for.
#' @export
#' @return The predicted values for the new_data passed in.
#' @examples
#' mod_predicts <- predict(fit_mod, new_data)
predict.model <- function(mod,new_data) {
  preds <- predictmodel(mod,new_data)
  return (preds)
}

#' This specifies what to do when predict is called on an object of class "modelfamily". This will
#' call the function predictmodelfamily on the fitted model family with it's corresponding parameters to
#' predict the values for the new data using the best model in the fitted model family.
#' @param mod The model famil object. This model family must be fitted.
#' @param newdata The new data you want to predict values for.
#' @export
#' @return The predicted values for the new_data passed in.
#' @examples
#' mod_family_predicts <- predict(fitted_model_fam, new_data)
predict.modelfamily <- function(mod,new_data) {
  preds <- predictmodelfamily(mod,new_data)
  return (preds)
}
