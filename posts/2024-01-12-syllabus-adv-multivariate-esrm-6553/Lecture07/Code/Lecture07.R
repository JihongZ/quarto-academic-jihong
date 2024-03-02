library(tidyverse)
library(here)
library(cmdstanr)

self_color <- c("#DB7093", "#AFEEEE", "#3CB371", "#9370DB", "#FFD700")
root_dir <- "posts/2024-01-12-syllabus-adv-multivariate-esrm-6553/Lecture07/Code"
save_dir <- "~/Library/CloudStorage/OneDrive-Personal/2024 Spring/ESRM6553 - Advanced Multivariate Modeling/Lecture07"
dat <- read.csv(here(root_dir, 'conspiracies.csv'))
itemResp <- dat[,1:10]
colnames(itemResp) <- paste0('item', 1:10)
itemResp |> 
  rownames_to_column("ID") |> 
  pivot_longer(-ID, names_to = "Item", values_to = "Response") |> 
  mutate(Item = factor(Item, levels = paste0('item', 1:10)),
         Response = factor(Response, levels = 1:5)) |> 
  ggplot() +
  geom_bar(aes(x = Response, fill = Response, group = Response), 
           position = position_stack()) +
  facet_wrap(~ Item, nrow = 3, ncol = 4) +
  theme_classic() +
  scale_fill_manual(values = self_color)

## Prior density function plots
set.seed(1234)
data.frame(
  x = seq(0, 5, .001),
  y = dnorm(x = seq(0, 5, .001), mean = 0, sd = sqrt(1000))
) |> 
  ggplot(aes(x=x, y =y)) +
  geom_path() +
  labs(x = "", y = "Probability") +
  theme_classic() +
  theme(text = element_text(size = 17))

data.frame(
  x = seq(0, 2, .001),
  y = dexp(x = seq(0, 2, .001), rate = .01)
  ) |> 
  ggplot(aes(x=x, y =y)) +
  geom_path() +
  labs(x = "", y = "Probability") +
  theme_classic() +
  theme(text = element_text(size = 17))

# R's data list object
# data dimensions
conspiracyItems = itemResp
nObs = nrow(conspiracyItems)
nItems = ncol(conspiracyItems)

# item intercept hyperparameters
muMeanHyperParameter = 0
muMeanVecHP = rep(muMeanHyperParameter, nItems)

muVarianceHyperParameter = 1000
muCovarianceMatrixHP = diag(x = muVarianceHyperParameter, nrow = nItems)

# item discrimination/factor loading hyperparameters
lambdaMeanHyperParameter = 0
lambdaMeanVecHP = rep(lambdaMeanHyperParameter, nItems)

lambdaVarianceHyperParameter = 1000
lambdaCovarianceMatrixHP = diag(x = lambdaVarianceHyperParameter, nrow = nItems)

# unique standard deviation hyperparameters
psiRateHyperParameter = .01
psiRateVecHP = rep(.1, nItems)

modelCFA_data = list(
  nObs = nObs,
  nItems = nItems,
  Y = conspiracyItems, 
  meanMu = muMeanVecHP,
  covMu = muCovarianceMatrixHP,
  meanLambda = lambdaMeanVecHP,
  covLambda = lambdaCovarianceMatrixHP,
  psiRate = psiRateVecHP
)

modelCFA_stan <- cmdstan_model(here(root_dir, "Lecture07.stan"))

modelCFA_samples = modelCFA_stan$sample(
  data = modelCFA_data,
  seed = 09102022,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

modelCFA_samples$summary(c("mu", "lambda", "psi")) 
modelCFA_samples$save_object(here(save_dir, "model01.RDS"))
