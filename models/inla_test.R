library(INLA)
model_data <- train_data

model_data$def_naturaleza <- relevel(as.factor(model_data$def_naturaleza), ref = "Violencia interpersonal")
model_data$dia_del_hecho <- relevel(as.factor(model_data$dia_del_hecho), ref = "semana")


# Prepare the data for INLA's multinomial model
# INLA requires the response to be in a matrix format with one column per category
response_matrix <- model.matrix(~ def_naturaleza - 1, data = model_data)
colnames(response_matrix) <- levels(model_data$def_naturaleza)

# Create indices for the different categories
n_categories <- ncol(response_matrix)
n_obs <- nrow(model_data)
category_indices <- rep(1:n_categories, each = n_obs)

# Create the expanded dataset for INLA
inla_data <- data.frame(
  y = as.vector(response_matrix),
  category = category_indices,
  id = rep(1:n_obs, times = n_categories)
)

# Add the predictors to the expanded dataset
predictors <- model_data %>% 
  select(-def_naturaleza) %>% 
  slice(rep(1:n(), each = n_categories))

inla_data <- cbind(inla_data, predictors)

# Define the formula for the model
# We'll include all available predictors
formula <- y ~ -1 + 
  f(category, model = "iid", hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +
  f(id, model = "iid", hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +

  ciclo_vital_ + escolaridad 
# Fit the INLA model
inla_result <- inla(
  formula,
  family = "Poisson", # Using Poisson trick for multinomial
  data = inla_data,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  verbose = TRUE
)
save_data(test_data, "test_data")
saveRDS(inla_result, "inla_model.rds")
# Summary of the model
summary(inla_result)
inla_result$summary.fixed
print(inla_result$mlik)

par(mfrow = c(3, 3))
for (i in 1:nrow(inla_result$summary.hyperpar)) {
  plot(inla_result$marginals.hyperpar[[i]], 
       type = "l", 
       main = rownames(inla_result$summary.hyperpar)[i],
       xlab = "Value", ylab = "Density")
}

cpo <- inla_result$cpo$cpo
cat("Sum of log(CPO):", sum(log(cpo), na.rm = TRUE), "\n")
cat("Number of failures:", sum(inla_result$cpo$failure), "\n")


pred_probs <- matrix(inla_result$summary.fitted.values$mean, ncol = n_categories, byrow = TRUE)
colnames(pred_probs) <- levels(model_data$def_naturaleza)

# Compare with observed proportions
observed_props <- prop.table(table(model_data$def_naturaleza))
predicted_props <- colMeans(pred_probs)

comparison <- data.frame(
  Category = names(observed_props),
  Observed = as.numeric(observed_props),
  Predicted = as.numeric(predicted_props)
)

ggplot(comparison, aes(x = Category)) +
  geom_bar(aes(y = Observed, fill = "Observed"), stat = "identity", position = "dodge", alpha = 0.7) +
  geom_bar(aes(y = Predicted, fill = "Predicted"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Observed vs Predicted Proportions", y = "Proportion") +
  scale_fill_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal()

# 7. Check residuals (for each category)
residuals <- response_matrix - pred_probs
par(mfrow = c(2, 2))
for (i in 1:n_categories) {
  hist(residuals[,i], main = colnames(response_matrix)[i], xlab = "Residuals")
}

# 8. Posterior predictive checks (simulate from posterior and compare to observed)
# This requires sampling from the posterior
inla_samples <- inla.posterior.sample(100, inla_result)

# Compare simulated vs observed counts (simplified check)
sim_counts <- sapply(inla_samples, function(x) {
  probs <- matrix(exp(x$latent[grep("Predictor", rownames(x$latent))]), ncol = n_categories, byrow = TRUE)
  probs <- probs / rowSums(probs)
  sim_data <- t(apply(probs, 1, function(p) rmultinom(1, 1, p)))
  colSums(sim_data)
})

# Calculate credible intervals
cred_intervals <- apply(sim_counts, 1, quantile, probs = c(0.025, 0.5, 0.975))
observed_counts <- colSums(response_matrix)

pp_check <- data.frame(
  Category = colnames(response_matrix),
  Observed = observed_counts,
  Median = cred_intervals["50%",],
  Lower = cred_intervals["2.5%",],
  Upper = cred_intervals["97.5%",]
)

ggplot(pp_check, aes(x = Category, y = Observed)) +
  geom_point(size = 3, color = "blue") +
  geom_point(aes(y = Median), size = 3, color = "red") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "red") +
  labs(title = "Posterior Predictive Check", y = "Count") +
  theme_minimal()