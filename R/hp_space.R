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
