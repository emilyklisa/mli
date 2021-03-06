% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_modelfamily.R
\name{fitmodelfamily.knn}
\alias{fitmodelfamily.knn}
\title{This function specifically handles fitting model families of class knn. This handles
everything that needs to be done when fitting a knn model family.}
\usage{
\method{fitmodelfamily}{knn}(mod, hp_space, hp_strategy, calibration)
}
\arguments{
\item{mod}{The modelfamily object.}

\item{hp_space}{The hyperparameter space. This specifies the hyperparameters in the model
family.}

\item{hp_strategy}{The hyperparameter strategy. This specifies how the hyperparameters
are selected when fitting the model family.}

\item{calibration}{The calibration. This specifies how each model will be assessed when
fitting the model family.}
}
\value{
The fitted knn model family. This is an object of classes "knn" and "modelfamily".
}
\description{
This function specifically handles fitting model families of class knn. This handles
everything that needs to be done when fitting a knn model family.
}
\examples{
f_mod_fam <- fitmodelfamily(mod = new_knn_modelfamily(data = iris,design = Species ~ .),
hp_space = hp_space(vals = c(5,3,1), metric = c("euclidian")),
hp_strategy = "grid_search",
calibration = calibration(score = "accuracy",sampling = "bootstrap",reps = 10))
}
