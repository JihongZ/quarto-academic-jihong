# Simulation Study: Model 1 -----------------------------------------------
# One-factor model without cross-loadings
library(tidyverse)
library(cmdstanr)
set.seed(1234)
N <- 1000
J <- 6
# parameters
psi <- .3 # factor correlation
sigma <- .1 # residual varaince
FS <- mvtnorm::rmvnorm(N, mean = c(0, 0), sigma = matrix(c(1, psi, psi, 1), 2, 2, byrow = T))
Lambda <- matrix(
  c(
    0.7, 0,
    0.5, 0,
    0.3, 0,
    0, 0.7,
    0, 0.5,
    0, 0.3
  ), 6, 2,
  byrow = T
)
mu <- matrix(rep(0.1, J), nrow = 1, byrow = T)
residual <- mvtnorm::rmvnorm(N, mean = rep(0, J), sigma = diag(sigma, J))
Y <- t(apply(FS %*% t(Lambda), 1, \(x) x + mu)) + residual

# lavaan
library(lavaan)
mod <- "
F1 =~ I1 + I2 + I3
F2 =~ I4 + I5 + I6
"
dat <- as.data.frame(Y)
colnames(dat) <- paste0('I', 1:6)
fit <- cfa(mod, data = dat, std.lv = TRUE)
summary(fit, fit.measures = TRUE)

# input variables
Q <- matrix(
  c(
    1, 0,
    1, 0,
    1, 0,
    0, 1,
    0, 1,
    0, 1
  ), 6, 2,
  byrow = T
)
Q

## Transform Q to location index
loc <- Q |>
  as.data.frame() |>
  rename(`1` = V1, `2` = V2) |> 
  rownames_to_column("Item") |>
  pivot_longer(c(`1`, `2`), names_to = "Theta", values_to = "q") |> 
  mutate(across(Item:q, as.numeric)) |> 
  filter(q == 1) |> 
  mutate(Index = 1:6) |> 
  as.matrix()

data_list <- list(
  N = 1000, # number of subjects/observations
  J = J, # number of items
  K = 2, # number of latent variables,
  Y = Y,
  Q = Q,
  
  jj = loc[,1], 
  kk = loc[,2],
  Ind = loc[,4],
  R = nrow(loc),
  
  
  meanMu = rep(0, J),
  covMu = diag(1000, J),
  meanTheta = rep(0, 2),
  covTheta = matrix(c(1, 0.3, 0.3, 1), 2, 2, byrow = T),
  meanLambda = rep(0, nrow(loc)),
  covLambda = diag(1000, nrow(loc))
)

mod_cfa_twofactor <- cmdstan_model(here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture06", "Code", "simulation_loc.stan"))

# quick check using pathfinder
fit_pf <- mod_cfa_twofactor$pathfinder(data = data_list, seed = 1234, draws = 4000)
fit_pf$summary('lambda')


fit_cfa_twofactor <- mod_cfa_twofactor$sample(
  data = data_list,
  seed = 1234,
  chains = 4,
  parallel_chains = 4, 
  iter_sampling = 4000,
  iter_warmup = 2000
)

fit_cfa_twofactor$summary("lambda")
fit_cfa_twofactor$summary("mu")
fit_cfa_twofactor$summary("sigma")

save(Y, fit, mod_cfa_twofactor, fit_cfa_twofactor, Q, loc, data_list,
     file = here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture06", "Code", "Lecture06.RData")
)

