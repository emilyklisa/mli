predictmodel <- function(mod,new_data) UseMethod("predictmodel")
predictmodel.default <- function(mod,new_data) {
  cat("in predictmodel.default\n")
  invisible(NULL)
}
predictmodel.knn <- function(mod,new_data) {
  #there is no way to actually do anything with the metric for now
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
