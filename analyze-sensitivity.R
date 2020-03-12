#!/usr/bin/env Rscript --vanilla

library(tidyverse)

source("util.R")
source("model.R")

# Load estimates and variates
estimates <- c(
  readRDS("cache/estimates.rds"),
  list(u = 0.5)
)

scale_element <- function(lst, elt, by) `[[<-`(lst, elt, lst[[elt]] * by)

sens_f <- function(input, output, step = 0.1) {
  x0 <- estimates
  xl <- scale_element(x0, input, 1 - step / 2)
  xu <- scale_element(x0, input, 1 + step / 2)

  y0 <- model_call(x0)[[output]]
  yl <- model_call(xl)[[output]]
  yu <- model_call(xu)[[output]]

  (yu - yl) / y0
}

sens_results <- crossing(
  param = model_params,
  outcome = model_outcomes
) %>%
  mutate(sensitivity = map2_dbl(param, outcome, sens_f)) %>%
  arrange(sensitivity)

write_tsv(sens_results, "output/sensitivity.tsv")
