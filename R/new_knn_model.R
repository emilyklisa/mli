#' This function creates a knn model object.
#' @param data Defines the data to be used for the knn model object.
#' @export design Defines the design used for the knn model object.
#' @return The new knn model object. This is of classes "knn" and "model". This model object
#' has a boolean indicating it hasn't been fit, a default k value of -1, a default metric of
#' "euclidian, initialized empty values for predicted, truth, and train_data, and the data and
#' design fields set to be equal to the parameters passed into this function.
#' @examples
#' unfit_mod <- new_knn_model(data=iris, design = Species ~ .)
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
