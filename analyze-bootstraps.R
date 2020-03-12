#!/usr/bin/env Rscript --vanilla

library(tidyverse)

source("util.R")
source("model.R")

set.seed(5)
n_plot_points <- 1e3
max_nnt <- 10

# Load variates
variates <- readRDS("cache/variates.rds")

# Bootstrapping model
us <- c(0, 0.50, 1.0)

simulations <- tibble(u = us) %>%
  mutate(
    params = map(u, ~ `[[<-`(variates, "u", .)),
    outcomes = map(params, model_call)
  )

# CIs around the point estimates
cis <- simulations %>%
  select(u, outcomes) %>%
  unnest_longer(outcomes, values_to = "values", indices_to = "param") %>%
  mutate(
    estimate = map_dbl(values, mean),
    lci = map_dbl(values, ~ quantile(., ci_quantiles[1])),
    uci = map_dbl(values, ~ quantile(., ci_quantiles[2]))
  ) %>%
  select(u, param, estimate, lci, uci)

write_tsv(cis, "output/outcomes-cis.tsv")

# Plot of points
simulation_data <- simulations %>%
  select(-u) %>%
  unnest_wider(params) %>%
  unnest_wider(outcomes) %>%
  unnest(cols = c(model_params, model_outcomes))

exclusion_counts <- simulation_data %>%
  count(between(nnt, 0, max_nnt)) %>%
  mutate(f = n / sum(n))

write_tsv(exclusion_counts, "output/exclusion-counts.tsv")

plot_data <- simulation_data %>%
  # keep non-weird NNTs
  filter(between(nnt, 0, max_nnt)) %>%
  # rescale
  mutate_at(c("Cdot", "n_fmts"), ~ . / 1e3) %>%
  # subsample
  sample_n(n_plot_points) %>%
  pivot_longer(setdiff(model_params, c("u")), names_to = "param_name", values_to = "param_value") %>%
  pivot_longer(model_outcomes, names_to = "outcome_name", values_to = "outcome_value") %>%
  mutate(
    param_label = recode(param_name,
      Cdot = "C[symbol('\\267')]",
      p1 = "p[1]",
      p2 = "p[2]",
      pabx = "p[abx]",
      pfmt = "p[FMT]"
    ),
    outcome_label = recode(outcome_name,
      n_fmts = "N[FMT]",
      f_geq3 = "f['' >= 3]",
      nnt = "NNT"
    )
  )

plot <- plot_data %>%
  ggplot(aes(param_value, outcome_value, color = factor(u))) +
  facet_grid(outcome_label ~ param_label, scales = "free", labeller = label_parsed) +
  geom_point(shape = 3) +
  scale_color_manual(values = c("#edf8b1", "#7fcdbb", "#2c7fb8")) +
  xlab("Parameter value") +
  ylab("Outcome value") +
  guides(color = guide_legend("u")) +
  cowplot::theme_half_open() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))

ggsave("fig/outcomes.pdf")
ggsave("fig/outcomes.png")
