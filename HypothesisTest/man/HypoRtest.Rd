% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hypothesis_Test.R
\name{HypoRtest}
\alias{HypoRtest}
\title{Performs multiple hypothesis tests on a random sample}
\usage{
HypoRtest(dist, n, mu0, mu1 = NULL, alpha = 0.05, test1 = FALSE, test2 = FALSE)
}
\arguments{
\item{dist}{the distribution to sample from (either "normal", "uniform", "exponential", or "poisson")}

\item{n}{the sample size}

\item{mu0}{the hypothesized mean}

\item{mu1}{an optional second hypothesized mean for the difference in means test}

\item{alpha}{the significance level for the hypothesis tests}

\item{test1}{whether to perform a z-test in addition to a t-test}

\item{test2}{whether to perform a test for the difference in means}
}
\value{
a list containing the results of the hypothesis tests
}
\description{
Performs multiple hypothesis tests on a random sample
}
\examples{
result <- HypoRtest(dist = "exponential", n = 10, mu0 = 2,mu1 = 3, alpha = 0.05, test1 = TRUE, test2 = TRUE)
}
