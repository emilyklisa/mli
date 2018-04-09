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
