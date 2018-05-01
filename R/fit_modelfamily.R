#' This is a generic function that specifically handles fitting for model families. This handles
#' everything that needs to be done when fitting a model family. For objects that are not of a
#' defined class for model families this simply prints "in fitmodel.default".
#' @param mod The modelfamily object. This will likely be empty in the default case.
#' @param hp_space The hyperparameter space. This specifies the hyperparameters in the model
#' family. This will likely be empty in the default case.
#' @param hp_strategy The hyperparameter strategy. This specifies how the hyperparameters
#' are selected when fitting the model family. This will likely be empty in the default case.
#' @param calibration The calibration. This specifies how each model will be assessed when
#' fitting the model family. This will likely be empty in the default case.
#' @export
#' @examples
#' fitmodelfamily()
fitmodelfamily <- function(mod,hp_space,hp_strategy,calibration) UseMethod("fitmodelfamily")
fitmodelfamily.default <- function(mod,hp_space,hp_strategy,calibration) {
  cat("in fitmodelfamily.default\n")
  invisible(NULL)
}

#' This function specifically handles fitting model families of class knn. This handles
#' everything that needs to be done when fitting a knn model family.
#' @param mod The modelfamily object.
#' @param hp_space The hyperparameter space. This specifies the hyperparameters in the model
#' family.
#' @param hp_strategy The hyperparameter strategy. This specifies how the hyperparameters
#' are selected when fitting the model family.
#' @param calibration The calibration. This specifies how each model will be assessed when
#' fitting the model family.
#' @export
#' @return The fitted knn model family. This is an object of classes "knn" and "modelfamily".
#' @examples
#' f_mod_fam <- fitmodelfamily(mod = new_knn_modelfamily(data = iris,design = Species ~ .),
#' hp_space = hp_space(vals = c(5,3,1), metric = c("euclidian")),
#' hp_strategy = "grid_search",
#' calibration = calibration(score = "accuracy",sampling = "bootstrap",reps = 10))
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
