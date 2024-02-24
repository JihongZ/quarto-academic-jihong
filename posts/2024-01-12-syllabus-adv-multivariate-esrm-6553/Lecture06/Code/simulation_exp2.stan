data {
  int<lower=0> N; // number of observations
  int<lower=0> J; // number of items
  int<lower=0> K; // number of latent variables
  matrix[N, J] Y; // item responses
  
  int<lower=0> R; // number of rows in location matrix
  array[R] int<lower=0>jj;
  array[R] int<lower=0>kk;
  array[R] int<lower=0>q;
  
  //hyperparameter
  real<lower=0> meanSigma;
  real<lower=0> scaleSigma;
  vector[J] meanMu;
  matrix[J, J] covMu;      // prior covariance matrix for coefficients
  vector[K] meanTheta;
  matrix[K, K] corrTheta;
  array[2] int<lower=0> meanLambda;      // prior covariance matrix for coefficients
  array[2] int<lower=0> scaleLambda;      // prior covariance matrix for coefficients
}
parameters {
  vector<lower=0,upper=1>[J] mu;
  matrix<lower=0>[J, K] lambda;
  vector<lower=0,upper=1>[J] sigma; // the unique residual standard deviation for each item
  matrix[N, K] theta;                // the latent variables (one for each person)
  //matrix[K, K] corrTheta; // not use corrmatrix to avoid duplicancy of validation
}

model {
  mu ~ multi_normal(meanMu, covMu);
  // specify lambda's regulation
  for (r in 1:R) {
     lambda[jj[r], kk[r]] ~ cauchy(meanLambda[q[r]], scaleLambda[q[r]]);
  }
  //corrTheta ~ lkj_corr(eta);
  for (i in 1:N) {
    theta[i,] ~ multi_normal(meanTheta, corrTheta);
  }
  for (j in 1:J) {
    sigma[j] ~ cauchy(meanSigma, scaleSigma);                   // Prior for unique standard deviations
    Y[,j] ~ normal(mu[j]+theta*to_vector(lambda[j,]), sigma[j]);
  }
}
