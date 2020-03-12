#!/usr/bin/env Rscript --vanilla

library(tidyverse)

source("util.R")
source("model.R")

# Load variates
estimates <- readRDS("cache/estimates.rds")
variates <- readRDS("cache/variates.rds")

# Set a grid of coverage values
n_u <- (1e2 + 2)
u_values <- seq(0, 1, length.out = n_u)

# Baseline model
base_parms <- `[[<-`(estimates, "u", u_values)
base_outcomes <- model_call(base_parms)

base <- bind_cols(
  as_tibble(base_parms),
  as_tibble(base_outcomes)
)

# Bootstrapping model
us <- rep(u_values, length.out = n_variates)

params <- `[[<-`(variates, "u", us)
outcomes <- model_call(params)

simulations <- bind_cols(
  as_tibble(params),
  as_tibble(outcomes)
)

simulation_data <- simulations %>%
  group_by(u) %>%
  summarize(
    y = mean(n_fmts),
    ymin = quantile(n_fmts, ci_quantiles[1]),
    ymax = quantile(n_fmts, ci_quantiles[2])
  )

cat("Coverage @ 10k FMTs")
simulation_data %>%
  arrange(abs(y - 10000))

plot <- simulation_data %>%
  ggplot(aes(u)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.5) +
  geom_line(aes(y = n_fmts), data = base, size = 1) +
  scale_x_continuous(
    expression(paste("FMT coverage ", (u))),
    expand = c(0, 0),
    labels = scales::percent
  ) +
  scale_y_continuous(
    expression(paste("No. annual FMTs ", (N[FMT]))),
    expand = c(0, 0),
    labels = function(x) prettyNum(x, big.mark = ","),
    breaks = 1:6 * 1e4
  ) +
  cowplot::theme_half_open() +
  cowplot::background_grid() +
  theme(
    axis.title.x = element_text(margin = margin(10, 0, 0, 0, "pt")),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0, "pt")),
    plot.margin = margin(2, 20, 2, 2, "pt")
  )

for (ext in c("png", "pdf")) {
  ggsave(str_glue("fig/coverage.{ext}"), width = 8.9, height = 10, unit = "cm")
}
