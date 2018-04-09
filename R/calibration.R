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
