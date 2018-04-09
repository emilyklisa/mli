new_knn_model <- function(data=NULL, design=NULL)
{

  me <- list(
    beenFit = FALSE,
    data = data,
    design = design,
    predicted = NULL,
    truth = NULL,
    k = -1,
    metric = "euclidian"
  )

  ## Set the name for the class
  class(me) <- c("knn", "model")
  return(me)
}

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

hp_space <- function(vals=NULL, metric=NULL)
{
  me <- list(
    vals = vals,
    metric = metric
  )
  ## Set the name for the class
  class(me) <- c("hp_space")
  return(me)
}

calibration <- function(score="accuracy", sampling="bootstrap", reps=10)
{
  me <- list(
    score = score,
    sampling = sampling,
    reps = reps
  )
  ## Set the name for the class
  class(me) <- c("calibration")
  return(me)
}

new_knn_modelfamily <- function(data=NULL, design=NULL)
{

  me <- list(
    beenFit = FALSE,
    data = data,
    design = design,
    best_model = NULL
  )

  ## Set the name for the class
  class(me) <- c("knn", "modelfamily")
  return(me)
}

fitmodelfamily <- function(mod,hp_space,hp_strategy,calibration) UseMethod("fitmodelfamily")
fitmodelfamily.default <- function(mod,hp_space,hp_strategy,calibration) {
  cat("in fitmodelfamily.default\n")
  invisible(NULL)
}
fitmodelfamily.knn <- function(mod,hp_space,hp_strategy,calibration) {
  if (hp_strategy == "grid_search") {
    if (calibration$sampling == "bootstrap") {
      bstraps <- bootstraps(mod$data, times=calibration$reps)
      best_score <- 0.0
      for (i in 1: length(hp_space$vals)) {
        for (j in 1:length(hp_space$metric)) {
          #for each combination of hyperparameters and metrics
          scores <- c()
          #for each bootstrap sample
          for (k in 1:calibration$reps) {
            curmod <- new_knn_model(data=as.data.frame(bstraps$splits[[k]]), design = mod$design)
            curmodfitted <- fit(mod = curmod, k=hp_space$vals[i], metric=hp_space$metric[j])
            preds <- as.data.frame(curmodfitted$predicted)
            combiner <- cbind(curmodfitted$truth, preds)
            colnames(combiner) <- c("truth", "predicted")
            if (calibration$score == "accuracy") {
              cur_score <- accuracy(combiner, truth, predicted)
            }
            scores <- c(scores, cur_score)
          }
          avg_score <- mean(scores)
          if (avg_score > best_score) {
            best_score <- avg_score
            mod$best_model <- curmodfitted
          }

        }
      }
      mod$beenFit = TRUE
      return(mod)
    }
  }
}

fit <- function(mod,hp_space,hp_strategy,calibration, k,metric) UseMethod("fit")
fit.default <- function(mod,k,metric) {
  cat("in fit.default\n")
  invisible(NULL)
}
fit.model <- function(mod,hp_space,hp_strategy,calibration, k=-1,metric="euclidian") {
  ret_mod <- fitmodel(mod,k,metric)
  return (ret_mod)
}
fit.modelfamily <- function(mod,hp_space,hp_strategy,calibration,k,metric) {
  ret_mod <- fitmodelfamily(mod,hp_space,hp_strategy,calibration)
  return (ret_mod)
}

extract_model <-function(modfam) {
  return(modfam$best_model)
}

