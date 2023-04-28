#' Performs multiple hypothesis tests on a random sample
#'
#' @param dist the distribution to sample from (either "normal", "uniform", "exponential", or "poisson")
#' @param n the sample size
#' @param mu0 the hypothesized mean
#' @param mu1 an optional second hypothesized mean for the difference in means test
#' @param alpha the significance level for the hypothesis tests
#' @param test1 whether to perform a z-test in addition to a t-test
#' @param test2 whether to perform a test for the difference in means
#'
#' @return a list containing the results of the hypothesis tests
#' @export
#'
#' @examples
#' result <- HypoRtest(dist = "exponential", n = 10, mu0 = 2,mu1 = 3, alpha = 0.05, test1 = TRUE, test2 = TRUE)
HypoRtest <- function(dist, n, mu0, mu1 = NULL, alpha = 0.05,
                     test1 = FALSE, test2 = FALSE) {

  # Generate random sample from specified distribution
  set.seed(1234)
  sample <- switch(dist,
                   "normal" = rnorm(n),
                   "uniform" = runif(n),
                   "exponential" = rexp(n),
                   "poisson" = rpois(n, lambda = 5))

  # Calculate sample mean and standard deviation
  xbar <- mean(sample)
  s <- sd(sample)

  # Perform hypothesis test using t-test
  t_stat <- (xbar - mu0) / (s / sqrt(n))
  df <- n - 1
  p_value <- pt(t_stat, df = df, lower.tail = FALSE) * 2

  # Create result object
  result <- list(
    sample_mean = round(xbar, 2),
    sample_sd = round(s, 2),
    hypothesized_mean = mu0,
    t_statistic = round(t_stat, 2),
    degrees_of_freedom = df,
    two_tailed_p_value = format(p_value, scientific = FALSE),
    conclusion = ifelse(p_value < alpha,
                        "Reject Null Hypothesis",
                        "Fail to Reject Null Hypothesis")
  )

  # Perform additional hypothesis tests
  if (test1) {
    # Perform hypothesis test using z-test
    z_stat <- (xbar - mu0) / (s / sqrt(n))
    p_value_z <- pnorm(z_stat, lower.tail = FALSE) * 2
    result$z_test <- list(
      z_statistic = round(z_stat, 2),
      two_tailed_p_value = format(p_value_z, scientific = FALSE),
      conclusion = ifelse(p_value_z < alpha,
                          "Reject Null Hypothesis",
                          "Fail to Reject Null Hypothesis")
    )
  }

  if (test2) {
    # Perform hypothesis test for difference in means
    sample2 <- switch(dist,
                      "normal" = rnorm(n),
                      "uniform" = runif(n),
                      "exponential" = rexp(n, rate = 1/mu1),
                      "poisson" = rpois(n, lambda = mu1))
    xbar2 <- mean(sample2)
    s2 <- sd(sample2)
    t_stat2 <- (xbar - xbar2) / sqrt(s^2/n + s2^2/n)
    df2 <- 2*n - 2
    p_value2 <- pt(t_stat2, df = df2, lower.tail = FALSE) * 2
    result$mean_diff_test <- list(
      sample2_mean = round(xbar2, 2),
      sample2_sd = round(s2, 2),
      hypothesized_mean_diff = mu0 - mu1,
      t_statistic = round(t_stat2, 2),
      degrees_of_freedom = df2,
      two_tailed_p_value = format(p_value2, scientific = FALSE),
      conclusion = ifelse(p_value2 < alpha,
                          "Reject Null Hypothesis",
                          "Fail to Reject Null Hypothesis")
    )
  }

  # Return result object
  return(result)
}
