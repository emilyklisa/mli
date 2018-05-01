#' This is a generic function that specifically handles predicting for model families. This handles
#' everything that needs to be done when predicting a model family. For objects that do not have a
#' defined class that specifies the kind of model family this simply prints "in predictmodelfamily.default".
#' @param mod The model family object. This will likely be empty in the default case.
#' @param newdata The new data you want to predict values for. This will likely be empty in
#' the default case.
#' @export
#' @examples
#' predictmodelfamily()
predictmodelfamily <- function(mod,new_data) UseMethod("predictmodelfamily")
predictmodelfamily.default <- function(mod,new_data) {
  cat("in predictmodelfamily.default\n")
  invisible(NULL)
}

#' This function specifically handles predicting knn model families. This handles
#' everything that needs to be done when predicting a knn model family.
#' @param mod The model family object.
#' @param newdata The new data you want to predict values for.
#' @export
#' @return The predicted values for the new_data passed in.
#' @examples
#' predictmodelfamily(fitted_model_fam, new_data)
predictmodelfamily.knn <- function(mod,new_data) {
  if (mod$beenFit) {
    best_mod = mod$best_model
    a<- best_mod$design
    b <-all.vars(a)[1]
    t <- select(best_mod$train_data, -starts_with(b))
    knn_preds <- class::knn(test = new_data,
                            train = t,
                            cl = best_mod$train_data[[b]],
                            k = best_mod$k)
    return(knn_preds)
  } else {
    return("You must fit the model family before making predictions")
  }
}


