data {
  int<lower=1> N;

  // Variable respuesta
  array[N] int<lower=0, upper=1> ac_mental;

  // Índices de predictores categóricos
  array[N] int<lower=1, upper=2> mujer_cabf;        // No, Sí
  array[N] int<lower=1, upper=4> def_naturaleza;    // 4 niveles
  array[N] int<lower=1, upper=3> sexo_agre;         // M, F, I
  array[N] int<lower=1, upper=2> conv_agre;         // No, Sí
  array[N] int<lower=1, upper=3> ciclo_vital;       // 3 niveles
  array[N] int<lower=1, upper=33> departamento;     // 33 niveles
}

parameters {
  real alpha;   // intercepto global (log-odds promedio)

  // Efectos categóricos con restricción suma-cero
  sum_to_zero_vector[2]  beta_mujer;
  sum_to_zero_vector[4]  beta_naturaleza;
  sum_to_zero_vector[3]  beta_sexo_agre;
  sum_to_zero_vector[2]  beta_conv_agre;
  sum_to_zero_vector[3]  beta_ciclo_vital;
  sum_to_zero_vector[33] beta_departamento;
}

model {
  // Priors
  alpha ~ normal(0, 5);

  beta_mujer         ~ normal(0, 1);
  beta_naturaleza    ~ normal(0, 1);
  beta_sexo_agre     ~ normal(0, 1);
  beta_conv_agre     ~ normal(0, 1);
  beta_ciclo_vital   ~ normal(0, 1);
  beta_departamento  ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    ac_mental[n] ~ bernoulli_logit(
      alpha
      + beta_mujer[mujer_cabf[n]]
      + beta_naturaleza[def_naturaleza[n]]
      + beta_sexo_agre[sexo_agre[n]]
      + beta_conv_agre[conv_agre[n]]
      + beta_ciclo_vital[ciclo_vital[n]]
      + beta_departamento[departamento[n]]
    );
  }
}
