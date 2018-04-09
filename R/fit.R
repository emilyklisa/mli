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
