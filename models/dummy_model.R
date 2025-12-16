
set.seed(123)

# Tamaño de la muestra
N <- 2000

# Variable categórica con 4 niveles
K <- 4
levels_cat <- c("A", "B", "C", "D")

cat_var <- factor(
  sample(levels_cat, N, replace = TRUE),
  levels = levels_cat
)

# Efectos verdaderos (suman 0)
beta_true <- c(0.8, -0.3, -0.1, -0.4)
sum(beta_true)  # 0

# Intercepto verdadero
alpha_true <- -0.2

# Índice numérico
cat_idx <- as.integer(cat_var)

# Generar respuesta binaria
eta <- alpha_true + beta_true[cat_idx]
p <- plogis(eta)
y <- rbinom(N, size = 1, prob = p)

# Base de datos final
df <- data.frame(
  y = y,
  categoria = cat_var
)

head(df)

stan_data <- list(
  N = N,
  K = K,
  y = y,
  cat_idx = cat_idx
)

stan_code <- "
data {
  int<lower=1> N;
  int<lower=2> K;
  array[N] int<lower=0,upper=1> y;
  array[N] int<lower=1,upper=K> cat_idx;
}

parameters {
  real alpha;                      // intercepto (promedio global)
  sum_to_zero_vector[K] beta;      // efectos categóricos con suma = 0
}

model {
  // Priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(alpha + beta[cat_idx[n]]);
  }
}

"


model_test <- cmdstan_model(write_stan_file(stan_code))

fit_vb <- model_test$variational(
  data = stan_data,
  algorithm = "meanfield",
  output_samples = 2000,
  seed = 123
)

library(posterior)

draws <- as_draws_df(fit_vb$draws())

results <- summary(draws)

# Subset correcto de betas
beta_draws <- subset_draws(
  draws,
  variable = paste0("beta[", 1:K, "]")
)


