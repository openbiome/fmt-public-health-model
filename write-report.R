#!/usr/bin/env Rscript --vanilla

library(tidyverse)

source("model.R")

params <- read_tsv("output/parameters.tsv") %>%
  mutate(distribution = "beta") %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  bind_rows(
    tribble(
      ~param, ~distribution, ~estimate, ~lci, ~uci,
      "E", "uniform", E, E_min, E_max,
      "Cdot", "normal", Cdot, Cdot_lci, Cdot_uci
    )
  ) %>%
  mutate(range = str_glue("{lci} to {uci}")) %>%
  select(param, estimate, range, distribution)

outcomes <- read_tsv("output/outcomes-cis.tsv") %>%
  mutate_if(is.numeric, ~ signif(., 2)) %>%
  mutate(range = str_glue("{estimate} ({lci} to {uci})")) %>%
  select(param, u, range) %>%
  arrange(param, u, range)

sens <- read_tsv("output/sensitivity.tsv") %>%
  # lump together outcomes with the same sensitivity
  mutate_at("sensitivity", ~ round(., 6)) %>%
  group_by(param, sensitivity) %>%
  summarize(outcome = str_c(outcome, collapse = ", ")) %>%
  select(param, outcome, sensitivity) %>%
  arrange(sensitivity) %>%
  # nice rounding
  filter(abs(sensitivity) > 1e-3) %>%
  mutate_at("sensitivity", ~ signif(. * 100, 2))

output <- c(
  "PARAMETERS -----------------------",
  format_tsv(params),
  "OUTCOMES -------------------------",
  format_tsv(outcomes),
  "SENSITIVITY ----------------------",
  format_tsv(sens)
)

write_lines(output, "output/report.txt")
