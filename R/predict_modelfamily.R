predictmodelfamily <- function(mod,new_data) UseMethod("predictmodelfamily")
predictmodelfamily.default <- function(mod,new_data) {
  cat("in predictmodel.default\n")
  invisible(NULL)
}
predictmodelfamily.knn <- function(mod,new_data) {
  #there is no way to actually do anything with the metric for now
  if (mod$beenFit) {
    best_mod = mod$best_model
    a<- best_mod$design
    b <-all.vars(a)[1]
    print(best_mod$train_data)
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


