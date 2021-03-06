% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{likelihood_saem}
\alias{likelihood_saem}
\title{likelihood_saem}
\usage{
likelihood_saem(
  beta,
  mu,
  Sigma,
  Y,
  X.obs,
  rindic = as.matrix(is.na(X.obs)),
  whichcolXmissing = (1:ncol(rindic))[apply(rindic, 2, sum) > 0],
  mc.size = 2
)
}
\arguments{
\item{beta}{Estimated parameter of logistic regression model.}

\item{mu}{Estimated parameter \eqn{\mu}.}

\item{Sigma}{Estimated parameter \eqn{\Sigma}.}

\item{Y}{Response vector \eqn{N \times 1}{N * 1}}

\item{X.obs}{Design matrix with missingness \eqn{N \times p}{N * p}}

\item{rindic}{Missing pattern of X.obs. If a component in X.obs is missing, the corresponding position in rindic is 1; else 0.}

\item{whichcolXmissing}{The column index in covariate containing at least one missing observation.}

\item{mc.size}{Monte Carlo sampling size.}
}
\value{
Observed log-likelihood.
}
\description{
Used in main function miss.saem. Calculate the observed log-likelihood for logistic regression model with missing data, using Monte Carlo version of Louis formula.
}
\examples{
# Generate dataset
N <- 50  # number of subjects
p <- 3     # number of explanatory variables
mu.star <- rep(0,p)  # mean of the explanatory variables
Sigma.star <- diag(rep(1,p)) # covariance
beta.star <- c(1, 1,  0) # coefficients
beta0.star <- 0 # intercept
beta.true = c(beta0.star,beta.star)
X.complete <- matrix(rnorm(N*p), nrow=N)\%*\%chol(Sigma.star) +
              matrix(rep(mu.star,N), nrow=N, byrow = TRUE)
p1 <- 1/(1+exp(-X.complete\%*\%beta.star-beta0.star))
y <- as.numeric(runif(N)<p1)
# Generate missingness
p.miss <- 0.10
patterns <- runif(N*p)<p.miss #missing completely at random
X.obs <- X.complete
X.obs[patterns] <- NA

# Observed log-likelihood
ll_obs = likelihood_saem(beta.true,mu.star,Sigma.star,y,X.obs)
}
