data {
  int<lower=1> N;

  // Variable respuesta
  array[N] int<lower=0, upper=1> ac_mental;

  // Índices de predictores categóricos
  array[N] int<lower=1, upper=2> mujer_cabf;        // No, Sí
  array[N] int<lower=1, upper=4> def_naturaleza;    // 4 niveles
  array[N] int<lower=1, upper=3> sexo_agre;         // M, F, I
  array[N] int<lower=1, upper=2> conv_agre;         // No, Sí
  array[N] int<lower=1, upper=3> area;         // No, Sí
  array[N] int<lower=1, upper=2> escenario;         // Publico, Vivienda
  array[N] int<lower=1, upper=2> pac_hos;         // No, Sí
  array[N] int<lower=1, upper=3> ciclo_vital;       // 3 niveles
  array[N] int<lower=1, upper=5> region;     // 
}

parameters {
  real alpha;   // intercepto global (log-odds promedio)

  // Efectos categóricos con restricción suma-cero
  sum_to_zero_vector[2]  beta_mujer;
  sum_to_zero_vector[4]  beta_naturaleza;
  sum_to_zero_vector[3]  beta_sexo_agre;
  sum_to_zero_vector[2]  beta_conv_agre;
  sum_to_zero_vector[3]  beta_area;
  sum_to_zero_vector[2]  beta_escenario;
  sum_to_zero_vector[2]  beta_pac_hos;
  sum_to_zero_vector[3]  beta_ciclo_vital;
  sum_to_zero_vector[5]  beta_region;
}

model {
  // Priors
  alpha ~ normal(0, 5);

  beta_mujer         ~ normal(0, 1);
  beta_naturaleza    ~ normal(0, 1);
  beta_sexo_agre     ~ normal(0, 1);
  beta_conv_agre     ~ normal(0, 1);
  beta_area     ~ normal(0, 1);
  beta_escenario     ~ normal(0, 1);
  beta_pac_hos     ~ normal(0, 1);
  beta_ciclo_vital   ~ normal(0, 1);
  beta_region  ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    ac_mental[n] ~ bernoulli_logit(
      alpha
      + beta_mujer[mujer_cabf[n]]
      + beta_naturaleza[def_naturaleza[n]]
      + beta_sexo_agre[sexo_agre[n]]
      + beta_conv_agre[conv_agre[n]]
      + beta_area[area[n]]
      + beta_escenario[escenario[n]]
      + beta_pac_hos[pac_hos[n]]
      + beta_ciclo_vital[ciclo_vital[n]]
      + beta_region[region[n]]
    );
  }
}


generated quantities {

  // Varianzas por cada predictor (ANOVA bayesiano)
  real<lower=0> S_mujer;
  real<lower=0> S_naturaleza;
  real<lower=0> S_sexo_agre;
  real<lower=0> S_conv_agre;
  real<lower=0> S_area;
  real<lower=0> S_escenario;
  real<lower=0> S_pac_hos;
  real<lower=0> S_ciclo_vital;
  real<lower=0> S_region;

  // -------------------------
  // MATRICES DE CENTRADO
  // -------------------------

  // Mujer (2 niveles)
  matrix[2,2] M1;
  row_vector[2] bM = rep_vector(1.0, 2)';

  // Naturaleza (4 niveles)
  matrix[4,4] N1;
  row_vector[4] bN = rep_vector(1.0, 4)';

  // Sexo agresor (3 niveles)
  matrix[3,3] SA1;
  row_vector[3] bSA = rep_vector(1.0, 3)';

  // Convivencia agresor (2 niveles)
  matrix[2,2] C1;
  row_vector[2] bC = rep_vector(1.0, 2)';

  // Área (3 niveles)
  matrix[3,3] A1;
  row_vector[3] bA = rep_vector(1.0, 3)';

  // Escenario (2 niveles)
  matrix[2,2] E1;
  row_vector[2] bE = rep_vector(1.0, 2)';

  // Hospitalización paciente (2 niveles)
  matrix[2,2] H1;
  row_vector[2] bH = rep_vector(1.0, 2)';

  // Ciclo vital (3 niveles)
  matrix[3,3] CV1;
  row_vector[3] bCV = rep_vector(1.0, 3)';

  // Region (5 niveles)
  matrix[33,33] D1;
  row_vector[33] bD = rep_vector(1.0, 5)';

  // -------------------------
  // COMPLETAR LAS MATRICES
  // -------------------------

  for (j in 1:2)  M1[j]  = bM;
  for (j in 1:4)  N1[j]  = bN;
  for (j in 1:3)  SA1[j] = bSA;
  for (j in 1:2)  C1[j]  = bC;
  for (j in 1:3)  A1[j]  = bA;
  for (j in 1:2)  E1[j]  = bE;
  for (j in 1:2)  H1[j]  = bH;
  for (j in 1:3)  CV1[j] = bCV;
  for (j in 1:5) D1[j]  = bD;

  // -------------------------
  // ANOVA BAYESIANO
  // -------------------------

  S_mujer =
    sqrt(
      (1.0 / (2 - 1))
      * beta_mujer'
      * (diag_matrix(rep_vector(1.0, 2)) - (1.0 / 2) * M1)
      * beta_mujer
    );

  S_naturaleza =
    sqrt(
      (1.0 / (4 - 1))
      * beta_naturaleza'
      * (diag_matrix(rep_vector(1.0, 4)) - (1.0 / 4) * N1)
      * beta_naturaleza
    );

  S_sexo_agre =
    sqrt(
      (1.0 / (3 - 1))
      * beta_sexo_agre'
      * (diag_matrix(rep_vector(1.0, 3)) - (1.0 / 3) * SA1)
      * beta_sexo_agre
    );

  S_conv_agre =
    sqrt(
      (1.0 / (2 - 1))
      * beta_conv_agre'
      * (diag_matrix(rep_vector(1.0, 2)) - (1.0 / 2) * C1)
      * beta_conv_agre
    );

  S_area =
    sqrt(
      (1.0 / (3 - 1))
      * beta_area'
      * (diag_matrix(rep_vector(1.0, 3)) - (1.0 / 3) * A1)
      * beta_area
    );

  S_escenario =
    sqrt(
      (1.0 / (2 - 1))
      * beta_escenario'
      * (diag_matrix(rep_vector(1.0, 2)) - (1.0 / 2) * E1)
      * beta_escenario
    );

  S_pac_hos =
    sqrt(
      (1.0 / (2 - 1))
      * beta_pac_hos'
      * (diag_matrix(rep_vector(1.0, 2)) - (1.0 / 2) * H1)
      * beta_pac_hos
    );

  S_ciclo_vital =
    sqrt(
      (1.0 / (3 - 1))
      * beta_ciclo_vital'
      * (diag_matrix(rep_vector(1.0, 3)) - (1.0 / 3) * CV1)
      * beta_ciclo_vital
    );

  S_region =
    sqrt(
      (1.0 / (5 - 1))
      * beta_region'
      * (diag_matrix(rep_vector(1.0, 5)) - (1.0 / 5) * D1)
      * beta_region
    );
}
