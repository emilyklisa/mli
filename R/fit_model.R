#' This is a generic function that specifically handles fitting for models. This handles
#' everything that needs to be done when fitting a model. For objects that do not have a
#' defined class that specifies the kind of model this simply prints "in fitmodel.default".
#' @param mod The model object. This will likely be empty in the default case.
#' @param k The k value. This will likely be empty in the default case.
#' @param metric The metric. This will likely be empty in the default case.
#' @export
#' @examples
#' fitmodel()
fitmodel <- function(mod,k,metric) UseMethod("fitmodel")
fitmodel.default <- function(mod,k,metric) {
  cat("in fitmodel.default\n")
  invisible(NULL)
}

#' This function specifically handles fitting knn models. This handles
#' everything that needs to be done when fitting a knn model.
#' @param mod The knn model.
#' @param k The k value for the model. The default in this case in -1 which in the function
#' is changed to the calculation of the square root of the number of rows of data.
#' @param metric The metric. The default is set to "euclidian".
#' @export
#' @return The fitted knn model. This is an object of classes "knn" and "model".
#' @examples
#' fitmodel(mod = new_knn_model(data=iris, design = Species ~ .), k=3, metric="euclidian")
fitmodel.knn <- function(mod, k=-1, metric='euclidian') {
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
    t <- select(train, -starts_with(b))
    mod$train_data <- train
    knn_preds <- class::knn(test = test,
                            train = t,
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
    t <- select(train, one_of(vector_of_vars))
    print(t)
    mod$train_data <- train
    knn_preds <- class::knn(test = test,
                            train = t,
                            cl = train[[b]],
                            k = mod$k)
  }
  mod$beenFit <- TRUE
  mod$predicted <- knn_preds
  return(mod)
}
