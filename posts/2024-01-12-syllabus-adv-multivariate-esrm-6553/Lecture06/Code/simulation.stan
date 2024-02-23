data {
  int<lower=0> N; // number of observations
  int<lower=0> J; // number of items
  int<lower=0> K; // number of latent variables
  matrix[N, J] Y; // item responses
  
  //int<lower=0> R; // number of rows in location matrix
  //array[R] int<lower=0>jj; // index of item in location matrix
  //array[R] int<lower=0>kk; // index of latent variables in location matrix
  matrix[J, K] Q;
  
  //hyperparameter
  vector[J] meanMu;
  matrix[J, J] covMu;      // prior covariance matrix for coefficients
  vector[K] meanTheta;
  matrix[K, K] covTheta;      // prior covariance matrix for coefficients
  vector[K] meanLambda;
  matrix[K, K] covLambda;      // prior covariance matrix for coefficients
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[J] mu;
  matrix[J, K] lambda;
  vector<lower=0>[J] psi; // the unique residual standard deviation for each item
  matrix[N, K] theta;                // the latent variables (one for each person)
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu ~ multi_normal(meanMu, covMu);
  psi ~ exponential(0.1);                   // Prior for unique standard deviations
  for (i in 1:N) {
    theta[i,] ~ multi_normal(meanTheta, covTheta);
  }
  for (j in 1:J) {
    for (k in 1:K) {
        lambda[j,K] ~ normal(0, 1);
    }
    Y[,j] ~ normal(mu[j]+ theta*to_vector(lambda[j,].*Q[j,]), psi[j]);
  }
}

