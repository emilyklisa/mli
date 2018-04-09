new_knn_model <- function(data=NULL, design=NULL)
{

  me <- list(
    beenFit = FALSE,
    data = data,
    design = design,
    predicted = NULL,
    truth = NULL,
    k = -1,
    train_data = NULL,
    metric = "euclidian"
  )

  ## Set the name for the class
  class(me) <- c("knn", "model")
  return(me)
}
