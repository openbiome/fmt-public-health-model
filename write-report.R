#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(scales)

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
  mutate(range = str_glue("{point_estimate} ({lci} to {uci})")) %>%
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

# No. FMTs at 15% uptake ----------------------------------------------
# Make linear interpolations of N_FMT vs. coverage (for estimate and CIs)
nfmt_fs <- read_tsv("output/coverage.tsv") %>%
  chop(cols = everything()) %>%
  pivot_longer(cols = -u) %>%
  mutate(f = map2(u, value, approxfun)) %>%
  with({ set_names(f, name) })

# Find place where N_FMT estimate hits 10k
u <- uniroot(
  function(x) nfmt_fs$y(x) - 1e4,
  c(0.01, 0.99)
)$root

nfmt <- signif(nfmt_fs$y(u), 2)
nfmt_min <- signif(nfmt_fs$ymin(u), 2)
nfmt_max <- signif(nfmt_fs$ymax(u), 2)

output <- c(
  "PARAMETERS -----------------------",
  format_tsv(params),
  "OUTCOMES -------------------------",
  format_tsv(outcomes),
  "SENSITIVITY ----------------------",
  format_tsv(sens),
  "UPTAKE @ 10K FMTS ----------------",
  str_glue("Coverage: {percent(u)}"),
  str_glue("N_FMT: {nfmt} 95% CI {nfmt_min} to {nfmt_max}")
)

write_lines(output, "output/report.txt")
