% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyperparam.J.R
\name{hyperparam.J}
\alias{hyperparam.J}
\title{Selecting optimal number of mixture components based on various criteria}
\usage{
hyperparam.J(data, icp.torus.objects, option = c("risk", "AIC", "BIC"))
}
\arguments{
\item{data}{n x d matrix of toroidal data on \eqn{[0, 2\pi)^d}
or \eqn{[-\pi, \pi)^d}}

\item{icp.torus.objects}{a list whose elements are icp.torus objects, generated by
\code{icp.torus.score}.}

\item{option}{a string one of "risk", "AIC", or "BIC", which determines the criterion
for the model selection. "risk" is based on the negative log-likelihood, "AIC" for the
Akaike Information Criterion, and "BIC" for the Bayesian Information Criterion.}
}
\value{
returns a list object which contains a \code{data.frame} for
  the evaluated criterion corresponding to each number of components, the optimal
  number of components, and the corresponding \code{icp.torus} object.
}
\description{
\code{hyperparam.J} evaluates criterion for each \code{icp.torus} objects, and select
  the optimal number of mixture components based on the evaluated criterion.
}
\examples{
\donttest{
data <- toydata2[, 1:2]
n <- nrow(data)
split.id <- rep(2, n)
split.id[sample(n, floor(n/2))] <- 1
Jvec <- 3:35
icp.torus.objects <- list()
for (j in Jvec){
  icp.torus.objects[[j]] <- icp.torus.score(data, split.id = split.id, method = "kmeans",
                                            kmeansfitmethod = "ge", init = "h",
                                            param = list(J = j), verbose = TRUE)
}
hyperparam.J(data, icp.torus.objects, option = "risk")
}
}
\references{
Akaike (1974), "A new look at the statistical model identification",
  Schwarz, Gideon E. (1978), "Estimating the dimension of a model"
}
\seealso{
\code{\link[ClusTorus]{icp.torus.score}}, \code{\link[ClusTorus]{hyperparam.torus}},
  \code{\link[ClusTorus]{hyperparam.alpha}}
}
