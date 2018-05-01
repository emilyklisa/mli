#' This function gives the best model from a fitted model family.
#' @param modfam The fitted model family.
#' @export
#' @return The best model in the fitted model family.
#' @examples
#' best_model <- extract_model(fitted_model_fam)
extract_model <-function(modfam) {
  return(modfam$best_model)
}

