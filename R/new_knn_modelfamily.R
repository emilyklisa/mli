#' This function creates a knn model family object.
#' @param data Defines the data to be used for the knn model family object.
#' @export design Defines the design used for the knn model family object.
#' @return The new knn model famil object. This is of classes "knn" and "modelfamily".
#'  This model object has a boolean indicating it hasn't been fit, initialized empty value
#'  for best_model, and the data and design fields set to be equal to the parameters
#'  passed into this function.
#' @examples
#' model_fam <- new_knn_modelfamily(data = iris,design = Species ~ .)
new_knn_modelfamily <- function(data=NULL, design=NULL)
{

  me <- list(
    beenFit = FALSE,
    data = data,
    design = design,
    best_model = NULL
  )

  ## Set the name for the class
  class(me) <- c("knn", "modelfamily")
  return(me)
}
