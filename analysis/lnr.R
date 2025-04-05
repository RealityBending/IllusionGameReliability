# Log-Normal Race (LNR) Model in R
# =============================================================
# This script implements a Log-Normal Race model for reaction times and errors.
# The model assumes that each accumulator draws a value from a LogNormal distribution
# (shifted by a non-decision time τ). The winning accumulator (minimum draw) determines
# the observed reaction time and choice.

# - Julia version: https://github.com/itsdfish/SequentialSamplingModels.jl/blob/master/src/LNR.jl
# - Reparametrization: https://github.com/paul-buerkner/brms/issues/1027

# Simulation ---------------------------------------------------------
#' Simulate data from a Log-Normal Race model with deviation-based parameterization.
#'
#' @param n Number of simulated trials.
#' @param mu The log-space mean (ν) for the baseline accumulator (choice 0).
#' @param mud The additive deviation (in log-space) from mu to obtain the mean for accumulator 1 (choice 1).
#' @param sigmazero The log-space standard deviation for the baseline accumulator (choice 0).
#' @param sigmad The log-deviation for the standard deviation so that the standard deviation for accumulator 1 is sigmazero * exp(sigmad).
#' @param tau A non-decision time (shift), τ (must be >= 0).
#'
#' @return A data.frame with columns:
#'         - rt: Reaction time (the minimum of the shifted lognormal draws).
#'         - choice: The index of the winning accumulator (0 for baseline, 1 for deviated).
#'
#' @examples
#' rez <- rlnr(10000, mu = 1, mud = 0.5, sigmazero = 1, sigmad = -0.5, tau = 0.2)
#' rez <- rez[rez$rt < 10, ]
#' hist(rez[rez$choice == 0, "rt"], breaks = seq(0, max(rez$rt) * 1.1, by = 0.1), xlab = "RT", main = "RTs for Choice 0 and 1")
#' hist(rez[rez$choice == 1, "rt"], breaks = seq(0, max(rez$rt) * 1.1, by = 0.1), col = rgb(1, 0, 0, 0.5), add = TRUE)
rlnr <- function(n, mu = 1, mud = 0, sigmazero = 1, sigmad = 0, tau = 0.2) {
  # Compute the means and standard deviations for both accumulators
  nu <- c(mu, mu + mud)
  sigma <- c(sigmazero, sigmazero * exp(sigmad))

  # Generate log-normal draws for both accumulators across all trials
  draws <- matrix(rlnorm(2 * n, meanlog = rep(nu, each = n), sdlog = rep(sigma, each = n)), nrow = n, ncol = 2) + tau

  # Determine choices and reaction times
  choice <- apply(draws, 1, which.min) - 1  # 0-based index
  rt <- draws[cbind(seq_len(n), choice + 1)]

  data.frame(rt = rt, choice = choice)
}

# Stan Code --------------------------------------------------------------------
# Define a stanvars object to pass the custom likelihood function to brms.
lnr_stanvars <- function() {
  brms::stanvar(scode = "
// Log-likelihood for a single observation from the reparameterized Log-Normal Race model.
// y: observed reaction time.
// dec: decision indicator (0 or 1).
// mu: baseline accumulator mean (in log-space) for choice 0.
// mud: additive deviation for the mean of choice 1.
// sigmazero: baseline accumulator standard deviation (in log-space) for choice 0.
// sigmad: log-deviation for the standard deviation of choice 1.
// tau: non-decision time (shift).
real lnr_lpdf(real y, real mu, real mud, real sigmazero, real sigmad, real tau, int dec) {
  real eps = 1e-8;
  real t_adj = y - tau;
  if (t_adj <= 0)
    return negative_infinity();

  // Convert 0-based decision to 1-based indexing for Stan.
  int dec_stan = dec + 1;

  // Construct means and standard deviations for the two accumulators.
  vector[2] nu;
  vector[2] sigma;
  nu[1] = mu;
  nu[2] = mu + mud;
  sigma[1] = sigmazero;
  sigma[2] = sigmazero * exp(sigmad);

  real lp = 0;
  // Sum contributions across both accumulators.
  for (i in 1:2) {
    if (i == dec_stan)
      lp += lognormal_lpdf(t_adj | nu[i], sigma[i]);
    else
      lp += log1m_exp(lognormal_lcdf(t_adj | nu[i], sigma[i]));
  }
  return lp;
}
", block = "functions")
}

# Custom Family ---------------------------------------------------------------
# Define the custom family for brms, with 'mu' as the baseline mean.
lnr <- function(link_mu = "identity", link_mud = "identity",
                link_sigmazero = "softplus", link_sigmad = "identity",
                link_tau = "softplus") {
  brms::custom_family(
    name = "lnr",
    dpars = c("mu", "mud", "sigmazero", "sigmad", "tau"),  # Distributional parameters
    links = c(link_mu, link_mud, link_sigmazero, link_sigmad, link_tau),  # Link functions
    vars = "dec[n]"  # Additional variable for decision
  )
}


# Posterior Predict -----------------------------------------------------------
# Simulate predicted outcomes using sampled parameters.
posterior_predict_lnr <- function(i, prep, ...) {
  mu        <- brms::get_dpar(prep, "mu", i = i)
  mud       <- brms::get_dpar(prep, "mud", i = i)
  sigmazero <- brms::get_dpar(prep, "sigmazero", i = i)
  sigmad    <- brms::get_dpar(prep, "sigmad", i = i)
  tau       <- brms::get_dpar(prep, "tau", i = i)

  n_draws <- length(tau)

  # Generate all predictions at once using the vectorized rlnr function
  sim_data <- rlnr(n_draws, mu = mu, mud = mud, sigmazero = sigmazero, sigmad = sigmad, tau = tau)

  as.matrix(sim_data)
}

# Test --------------------------------------------------------------------

# dat <- rlnr(3000, mu = 1, mud = 0, sigmazero = 1, sigmad = 0, tau = 0.2)
# fit2 <- brm(bf(rt | dec(choice) ~ 1), data = dat,
#             family = lnr(), stanvars = lnr_stanvars(),
#             init = function() list(mu = 1, mud = 0, sigmazero = 1, sigmad = 0, tau = 0.2, Intercept = 0),
#             refresh = 0, backend="cmdstanr", chains=4, iter = 2000)
#
# # Note: get_predicted() will use posterior_predict_lnr() internally
# pred <- insight::get_predicted(fit2, predict = "prediction", iterations = 100) |>
#   as.data.frame() |>
#   cbind(dat) |>
#   bayestestR::reshape_iterations() |>
#   mutate(iter_group = as.factor(iter_group))
#
#
# dat |>
#   mutate(choice = as.factor(choice)) |>
#   ggplot(aes(x = rt)) +
#   # geom_histogram(data=pred, aes(x=iter_value, group=iter_group), fill="black", alpha=0.01, bins = 200, position = "identity") +
#   # geom_density(data=pred, aes(x=iter_value, group=iter_group), color="black", alpha=0) +
#   # geom_density(aes(color = choice)) +
#   geom_histogram(aes(fill = choice), alpha=0.5, bins = 200, position = "identity") +
#   coord_cartesian(xlim = c(0, 20))

# insight::is_multivariate(m)
# x <- posterior_predict(fit2, ndraws = 10)[, 1:5]


# dat <- brms::rwiener(n = 1, alpha = 2, tau = .3, beta = .5, delta = .5 + x)
# dat$x <- rnorm(100, mean = 1)
# dat$rt <- dat$q
# dat$choice <- dat$resp
#
# fit1 <- brm(bf(rt | dec(choice) ~ 1), data = dat,
#               family = wiener(), refresh = 0, backend="cmdstanr", chains=1, iter = 300)
# brms::stancode(fit1)



