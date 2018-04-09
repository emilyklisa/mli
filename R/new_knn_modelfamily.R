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
