## Load packages
library(cmdstanr)
library(bayesplot)
## Read in data
dat <- read.csv(here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Data", "DietData.csv"))
dat$DietGroup <- factor(dat$DietGroup, levels = 1:3)
dat$HeightIN60 <- dat$HeightIN - 60
head(dat)

### Multivariate
set.seed(1234)

## 
mod_full_new <- cmdstan_model(here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture04", "Code","FullModel_New.stan"))
FullModelFormula = as.formula("WeightLB ~ HeightIN60 + DietGroup + HeightIN60*DietGroup")
X = model.matrix(FullModelFormula, data = dat)
data_full_new <- list(
  N = nrow(dat),
  P = ncol(X),
  X = X, 
  weightLB = dat$WeightLB,
  sigmaRate = 0.1
)
fit_full_new <- mod_full_new$sample(
  data = data_full_new,
  seed = 1234,
  chains = 4,
  parallel_chains = 4
)
fit_full_new$summary()
fit_full_new$time()


## Always save to RData


### PPP-values in Stan
mod_full_ppp <- cmdstan_model(here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture05", "Code","FullModel_PPP.stan"))
fit_full_ppp <- mod_full_ppp$sample(
  data = data_full_new,
  seed = 1234,
  chains = 4,
  parallel_chains = 4
)
fit_full_ppp$summary(variables = c('mean_weightLB_rep', 'sd_weightLB_rep', 'ppp_mean', 'ppp_sd'))

### WAIC, LOO
library(loo)
waic(fit_full_ppp$draws("log_lik"))
fit_full_ppp$loo('log_lik')



### WAIC/LOO in empty model
mod_empty_ppp <- cmdstan_model(here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture03", "Code","EmptyModel.stan"))
fit_empty_ppp <- mod_empty_ppp$sample(
  data = data_full_new,
  seed = 1234,
  chains = 4,
  parallel_chains = 4
)
fit_empty_ppp$loo('log_lik')

save(dat, fit_empty_ppp, fit_full_new, fit_full_ppp, file = here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture05", "Code","Lecture05.RData"))
