#' This function creates a calibration object. A calibration object is used in a model family
#' to specify how to compare models in a model family to determine the best model.
#' @param score Defines the score to be used for the calibration object. Defaults to 'accuracy'.
#' @export
#' @return The new calibration object. This is of class "calibration".
#' @examples
#' cal <- calibration(score = "accuracy",sampling = "bootstrap",reps = 10)
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
