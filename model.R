# Model variables and values

model_params <- c("p1", "p2", "pabx", "pfmt", "Cdot", "u", "E")
model_outcomes <- c("n_fmts", "f_geq3", "nnt")

# C. diff burden (Guh et al.)
Cdot <- 462100
Cdot_lci <- 428600
Cdot_uci <- 495600

E <- 15
E_min <- 10
E_max <- 20

# Model formulation

model_f <- function(Cdot, p1, p2, pabx, pfmt, u, E) {
  p3 <- (1 - u) * pabx + u * pfmt
  C1 <- Cdot / (1 + p1 + p1 * p2 * (1 - p3 ** (E - 2)) / (1 - p3))
  Cgeq3 <- C1 * p1 * p2 * (1 - p3 ** (E - 2)) / (1 - p3)
  n_fmts <- u * Cgeq3
  f_geq3 <- Cgeq3 / Cdot

  nnt <- (
    (
      1 / (1 - p3) - (E - 2) * p3 ** (E - 2) / (1 - p3 ** (E - 2))
    ) * (pabx - pfmt)
  ) ** (-1) - u

  list(n_fmts = n_fmts, f_geq3 = f_geq3, nnt = nnt)
}

model_call <- function(lst) do.call(model_f, lst)
