predict <- function(mod,new_data) UseMethod("predict")
predict.default <- function(mod,new_data) {
  cat("in predict.default\n")
  invisible(NULL)
}
predict.model <- function(mod,new_data) {
  preds <- predictmodel(mod,new_data)
  return (preds)
}
predict.modelfamily <- function(mod,new_data) {
  preds <- predictmodelfamily(mod,new_data)
  return (preds)
}
