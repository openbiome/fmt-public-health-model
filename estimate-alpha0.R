#!/usr/bin/env Rscript --vanilla

# Using the point estimates p and CIs from Rajasingham for the propotions of
# disease that are nonsevere, severe, or fulminant, find the value α_0 such
# that the Dirichlet distribution with concentration parameters p * α_0 best fit
# the CIs.

library(tidyverse)
library(gtools)

source("util.R")

raja_data <- read_tsv("data/raja-data.tsv") %>%
  filter(type == "dirichlet")

p <- raja_data$estimate
target <- with(raja_data, { c(lower, upper) })

obj <- function(alpha0) {
  variates <- rdirichlet(n_variates, p * alpha0)
  lci <- apply(variates, 2, function(x) quantile(x, ci_quantiles[1]))
  uci <- apply(variates, 2, function(x) quantile(x, ci_quantiles[2]))
  pred <- c(lci, uci)
  sum((pred - target) ** 2)
}

alpha0 <- optimize(obj, c(200, 400), tol = 1)$minimum

write_file(as.character(round(alpha0)), "cache/alpha0.txt")
