#' This is a generic function that specifically handles predicting for models. This handles
#' everything that needs to be done when predicting a model. For objects that do not have a
#' defined class that specifies the kind of model this simply prints "in predictmodel.default".
#' @param mod The model object. This will likely be empty in the default case.
#' @param newdata The new data you want to predict values for. This will likely be empty in
#' the default case.
#' @export
#' @examples
#' predictmodel()
predictmodel <- function(mod,new_data) UseMethod("predictmodel")
predictmodel.default <- function(mod,new_data) {
  cat("in predictmodel.default\n")
  invisible(NULL)
}

#' This function specifically handles predicting knn models. This handles
#' everything that needs to be done when predicting a knn model.
#' @param mod The model object.
#' @param newdata The new data you want to predict values for.
#' @export
#' @return The predicted values for the new_data passed in.
#' @examples
#' predictmodel(fit_mod, new_data)
predictmodel.knn <- function(mod,new_data) {
  if (mod$beenFit) {
    a<-mod$design
    b <-all.vars(a)[1]
    t <- select(mod$train_data, -starts_with(b))
    knn_preds <- class::knn(test = new_data,
                            train = t,
                            cl = mod$train_data[[b]],
                            k = mod$k)
    return(knn_preds)
  } else {
    return("You must fit the model before making predictions")
  }
}
