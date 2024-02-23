data {
  int<lower=0> N; // number of observations
  int<lower=0> J; // number of items
  int<lower=0> K; // number of latent variables
  matrix[N, J] Y; // item responses
  
  int<lower=0> R; // number of rows in location matrix
  array[R] int<lower=0>jj; // index of item in location matrix
  array[R] int<lower=0>kk; // index of latent variables in location matrix
  array[R] int<lower=0>Ind; // index of lambda
  //matrix[J, K] Q;
  
  //hyperparameter
  vector[J] meanMu;
  matrix[J, J] covMu;      // prior covariance matrix for coefficients
  vector[K] meanTheta;
  matrix[K, K] covTheta;      // prior covariance matrix for coefficients
  vector[R] meanLambda;
  matrix[R, R] covLambda;      // prior covariance matrix for coefficients
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[J] mu;
  vector<lower=0>[R] lambda;
  vector<lower=0>[J] sigma; // the unique residual standard deviation for each item
  matrix[N, K] theta;                // the latent variables (one for each person)
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu ~ multi_normal(meanMu, covMu);
  sigma ~ exponential(0.01);                   // Prior for unique standard deviations
  lambda ~ multi_normal(meanLambda, covLambda);
  
  for (i in 1:N) {
    theta[i,] ~ multi_normal(meanTheta, covTheta);
  }
  for (r in 1:R) {
    Y[,jj[r]] ~ normal(mu[jj[r]]+lambda[Ind[r]]*theta[,kk[r]], sigma[jj[r]]);
  }
}

