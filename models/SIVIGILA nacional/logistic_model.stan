data {
  int<lower=0> N;                     // Número de observaciones
  int<lower=0, upper=1> y[N];           // Variable respuesta (atención mental)
  int<lower=1> K;                      // Número de predictores
  matrix[N, K] X;                      // Matriz de diseño de predictores
}

parameters {
  real alpha;                          // Intercepto
  vector[K] beta;                      // Coeficientes de regresión
}


model {
  // Priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  
  // Modelo de regresión logística
  y ~ bernoulli_logit(alpha + X * beta);
}


generated quantities {
  vector[N] y_pred;
  
  for (i in 1:N) {
    y_pred[i] = bernoulli_logit_rng(alpha + X[i] * beta);
  }
}
