fitmodel <- function(mod,k,metric) UseMethod("fitmodel")
fitmodel.default <- function(mod,k,metric) {
  cat("in fitmodel.default\n")
  invisible(NULL)
}
fitmodel.knn <- function(mod, k=-1, metric='euclidian') {
  #there is no way to actually do anything with the metric for now
  if (k == -1) {
    mod$k <- as.integer(nrow(mod$data)^(1/2))
  } else {
    mod$k <- k
  }
  data <- initial_split(mod$data)

  train <- training(data)

  a<-mod$design

  if (all.vars(a)[2] == ".") {
    b <-all.vars(a)[1]
    test  <- select(testing(data), -starts_with(b))
    mod$truth <- select(testing(data), b)
    knn_preds <- class::knn(test = test,
                            train = select(train, -starts_with(b)),
                            cl = train[[b]],
                            k = mod$k)
  } else {
    vector_of_vars = c()
    b <-all.vars(a)[1]
    for (i in 2:length(all.vars(a))) {
      vector_of_vars[i-1] = all.vars(a)[i]
    }
    test <- select(testing(data), one_of(vector_of_vars))
    mod$truth <- select(testing(data), b)
    knn_preds <- class::knn(test = test,
                            train = select(train, one_of(vector_of_vars)),
                            cl = train[[b]],
                            k = mod$k)
  }
  mod$beenFit <- TRUE
  mod$predicted <- knn_preds
  return(mod)
}
