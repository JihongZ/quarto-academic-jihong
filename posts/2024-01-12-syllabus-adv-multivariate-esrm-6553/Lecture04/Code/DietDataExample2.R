## Load packages
library(cmdstanr)
library(bayesplot)
## Read in data
dat <- read.csv(here::here("posts", "2024-01-12-syllabus-adv-multivariate-esrm-6553", "Lecture03", "Code", "DietData.csv"))
dat$DietGroup <- factor(dat$DietGroup, levels = 1:3)
dat$HeightIN60 <- dat$HeightIN - 60
head(dat)

## Predicted data values
set.seed(1234)
P = 6
beta = matrix(data = runif(n = 6, min = 0, max = 10), nrow = P, ncol = 1)
beta
X = model.matrix(FullModelFormula, data = dat)
X %*% beta 

## Univariate vs. Multivariate variables
### Univariate
set.seed(1234)
beta0 = rnorm(100, 0, 1)
beta1 = rnorm(100, 0, 1)
cor(beta0, beta1)

### Multivariate
set.seed(1234)
sigma_of_betas = matrix(c(1, 0.5, 0.5, 1), ncol = 2)
betas = mvtnorm::rmvnorm(100, mean = c(0, 0), sigma = sigma_of_betas)
beta0 = betas[,1]
beta1 = betas[,2]
cor(beta0, beta1)

## Old Stan syntax
FullModelOld <- "
data{
    int<lower=0> N;
    vector[N] weightLB;
    vector[N] height60IN;
    vector[N] group2;
    vector[N] group3;
    vector[N] heightXgroup2;
    vector[N] heightXgroup3;
}
parameters {
  real beta0;
  real betaHeight;
  real betaGroup2;
  real betaGroup3;
  real betaHxG2;
  real betaHxG3;
  real<lower=0> sigma;
}
model {
  sigma ~ exponential(.1); // prior for sigma
  weightLB ~ normal(
    beta0 + betaHeight * height60IN + betaGroup2 * group2 + 
    betaGroup3*group3 + betaHxG2*heightXgroup2 +
    betaHxG3*heightXgroup3, sigma);
}
"
write.table(FullModelOld, "FullModel_Old.stan", row.names = FALSE, col.names = FALSE, quote = FALSE)
mod_full_old <- cmdstan_model("FullModel_Old.stan")
data_full_old <- list(
  N = nrow(dat),
  weightLB = dat$WeightLB,
  height60IN = dat$HeightIN60,
  group2 = as.numeric(dat$DietGroup == 2),
  group3 = as.numeric(dat$DietGroup == 3),
  heightXgroup2 = as.numeric(dat$DietGroup == 2) * dat$HeightIN60,
  heightXgroup3 = as.numeric(dat$DietGroup == 3) * dat$HeightIN60
)
fit_full_old <- mod_full_old$sample(
  data = data_full_old,
  seed = 1234,
  chains = 4,
  parallel_chains = 4
)
fit_full_old$summary()
fit_full_old$time()
################# On my computer, M1 Chip Mackbook pro
# $total
# [1] 0.1987331
# 
# $chains
# chain_id warmup sampling total
# 1        1  0.060    0.046 0.106
# 2        2  0.064    0.044 0.108
# 3        3  0.061    0.050 0.111
# 4        4  0.061    0.047 0.108
#################

## Old Stan syntax
FullModelNew <- "
data{
  int<lower=0> N;         // number of observations
  int<lower=0> P;         // number of predictors (plus column for intercept)
  matrix[N, P] X;         // model.matrix() from R 
  vector[N] weightLB;     // outcome
  real sigmaRate;         // hyperparameter: prior rate parameter for residual standard deviation
}
parameters {
  vector[P] beta;         // vector of coefficients for Beta
  real<lower=0> sigma;    // residual standard deviation
}
model {
  sigma ~ exponential(sigmaRate);         // prior for sigma
  weightLB ~ normal(X*beta, sigma);       // linear model
}
"
write.table(FullModelNew, "FullModel_New.stan", row.names = FALSE, col.names = FALSE, quote = FALSE)
mod_full_new <- cmdstan_model("FullModel_New.stan")
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

################# On my computer, M1 Chip Mackbook pro
# $total
# [1] 0.200011
# 
# $chains
# chain_id warmup sampling total
# 1        1  0.072    0.027 0.099
# 2        2  0.063    0.025 0.088
# 3        3  0.070    0.025 0.095
# 4        4  0.062    0.025 0.087
#################
lm(WeightLB ~ HeightIN60 + DietGroup + HeightIN60*DietGroup, data = dat)
