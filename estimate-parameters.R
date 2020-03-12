#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(gtools)

source("util.R")

# Parameters ----------------------------------------------------------

raja_data <- read_tsv("data/raja-data.tsv")

raja_estimates <- raja_data %>%
  { set_names(as.list(.$estimate), .$param) }

# Functions for drawing the variates ----------------------------------

# The Dirichlet variables (proportion nonsevere, severe, fulminant)

alpha0 <- as.numeric(read_file("cache/alpha0.txt"))

dirichlet_p <- raja_data %>%
  filter(type == "dirichlet") %>%
  pull(estimate)

dirichlet_name <- raja_data %>%
  filter(type == "dirichlet") %>%
  pull(param)

draw_dirichlet <- function(n) {
  raw_variates <- rdirichlet(n, alpha0 * dirichlet_p)
  variates_lst <- lapply(seq_along(dirichlet_p), function(i) raw_variates[, i])
  set_names(variates_lst, dirichlet_name)
}

dirichlet_variates <- draw_dirichlet(n_variates)

# Beta-distributed variables

fit_beta <- function(mu, lci, uci, interval = c(1e-6, 1e6)) {
  obj <- function(nu) {
    alpha <- mu * nu
    beta <- (1 - mu) * nu
    pred <- qbeta(ci_quantiles, alpha, beta)
    sum((pred - c(lci, uci)) ** 2)
  }

  nu <- optimize(obj, interval)$minimum
  alpha <- mu * nu
  beta <- (1 - mu) * nu

  list(alpha = alpha, beta = beta)
}

draw_beta_factory <- function(...) {
  fit <- fit_beta(...)
  function(n) rbeta(n, fit$alpha, fit$beta)
}

beta_variates <- raja_data %>%
  filter(type == "beta") %>%
  mutate(
    draw_beta = pmap(list(estimate, lower, upper), draw_beta_factory),
    variates = map(draw_beta, ~ (.)(n_variates))
  ) %>%
  { set_names(.$variates, .$param) }

variates <- c(dirichlet_variates, beta_variates)

# Functions for output variables --------------------------------------

p1 <- with(variates, {
  nonsevere_infection * ((1 - nonsevere_cure) + nonsevere_cure * nonsevere_recur) +
  severe_infection    * ((1 - severe_cure)    + severe_cure    * severe_recur   ) +
  fulminant_infection * (                       fulminant_cure * fulminant_recur)
})

p2   <- with(variates, { (1 - first_cure     ) + first_cure      * first_recur      })
pabx <- with(variates, { (1 - second_abx_cure) + second_abx_cure * second_abx_recur })
pfmt <- with(variates, { (1 - second_fmt_cure) + second_fmt_cure * second_fmt_recur })

results <- tibble(
  param = c("p1", "p2", "pabx", "pfmt"),
  variates = list(p1, p2, pabx, pfmt)
) %>%
  mutate(
    estimate = map_dbl(variates, mean),
    lci = map_dbl(variates, ~ quantile(., ci_quantiles[1])),
    uci = map_dbl(variates, ~ quantile(., ci_quantiles[2])),
    fit = pmap(list(estimate, lci, uci), fit_beta),
    alpha = map_dbl(fit, ~ .$alpha),
    beta = map_dbl(fit, ~ .$beta)
  ) %>%
  select(param, estimate, lci, uci, alpha, beta)

results

write_tsv(results, "output/parameters.tsv")
