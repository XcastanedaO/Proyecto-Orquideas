data {
  int<lower=2> K;           // number of outcome categories
  int<lower=0> N;           // number of observations
  int<lower=1> D;           // number of predictors (including intercept)
  int<lower=1,upper=K> y[N]; // outcome vector
  matrix[N, D] x;           // predictor matrix (including column of 1s for intercept)
}

parameters {
  matrix[K-1, D] beta;      // coefficients (K-1 sets)
}

model {
  // priors
  for (i in 1:(K-1)) {
    beta[i] ~ normal(0, 1); // weakly informative priors
  }
  
  // likelihood
  for (n in 1:N) {
    vector[K] eta;
    eta[1] = 0; // reference category
    for (k in 2:K) {
      eta[k] = x[n] * beta[k-1]';
    }
    y[n] ~ categorical_logit(eta);
  }
}

generated quantities {
  matrix[N, K] y_probs;
  for (n in 1:N) {
    vector[K] eta;
    eta[1] = 0;
    for (k in 2:K) {
      eta[k] = x[n] * beta[k-1]';
    }
    y_probs[n] = softmax(eta)';
  }
}