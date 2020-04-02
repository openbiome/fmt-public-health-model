#!/usr/bin/env Rscript --vanilla

library(tidyverse)

source("util.R")
source("model.R")

# Load parameters
params_tbl <- read_tsv("output/parameters.tsv") %>%
  mutate(variates = map2(alpha, beta, ~ rbeta(n_variates, .x, .y)))

# Primary episode cases from Guh et al.
draw_normal <- function(n, mu, lci, uci) {
  obj <- function(s) {
    pred <- qnorm(ci_quantiles, mu, s)
    sum((pred - c(lci, uci)) ** 2)
  }

  interval <- c(1e-6, uci - lci)

  s <- optimize(obj, interval)$minimum

  rnorm(n, mean = mu, sd = s)
}

Cdot_variates <- draw_normal(n_variates, Cdot, Cdot_lci, Cdot_uci)
E_variates <- runif(n_variates, E_min, E_max)

# Create point estimates and variates
estimates <- c(
  set_names(params_tbl$estimate, params_tbl$param),
  list(Cdot = Cdot, E = E)
)

variates <- c(
  set_names(params_tbl$variates, params_tbl$param),
  list(Cdot = Cdot_variates, E = E_variates)
)

saveRDS(estimates, "cache/estimates.rds")
saveRDS(variates, "cache/variates.rds")
