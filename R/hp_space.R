#' This function creates a hyperparameter space object. A hyperparameter space object is
#' used in model families to specify the hyperparameters in the model family.
#' @param vals Defines the hyperparameter values.
#' @export metric Defines the metric for the hyperparameter values.
#' @return The new hyperparameter space object. This is of class "hp_space".
#' @examples
#' cal <- calibration(score = "accuracy",sampling = "bootstrap",reps = 10)
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
