\name{RotationForest-package}
\alias{RotationForest-package}
\alias{RotationForest}
\docType{package}
\title{
An implementation of "Rotation Forest" decision trees in R
}
\description{
A RotationForest consists of an ensemble of decision trees (like a Random Forest), but every tree consists of k subsets and PCA is applied on each subset. This R package implements the presented analysis in Rodriguez et al. 2006 to provide the ability for further research to be done in the field.
}
\details{
\tabular{ll}{
Package: \tab RotationForest\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-14\cr
License: \tab MIT \cr
}
RotationForest consists of two key user-exposed functions: RotationForest(...) and predict.RotationForest(...), the first of which is a constructor for the forest and the second is a prediction utility. Read the documentation for each of these functions for more information. 
}
\author{
Manan Shah

Maintainer: Manan Shah <manan.shah.777@gmail.com>
}
\references{
Juan J. Rodriguez, Ludmila I. Kuncheva, and Carlos J. Alonso. Rotation Forest: A New
Classifier Ensemble Method. IEEE Transactions on Pattern Analysis and Machine Intelligence. 2006. \\
Ludmila I. Kuncheva and Juan J. Rodriguez. An experimental study on rotation forest
ensembles. In Proceedings of the 7th International Conference on Multiple Classifier
Systems, MCS'07, pages 459-468, Berlin, Heidelberg, 2007. Springer-Verlag.
}
\keyword{ml, rotationforest, randomforest, decisiontree}
