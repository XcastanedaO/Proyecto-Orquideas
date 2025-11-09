library(nimble)
X <- model.matrix(~ ., data = test_data[, -which(names(test_data) == "def_naturaleza")])
test_data$def_naturaleza <- factor(test_data$def_naturaleza,
                            levels = c(setdiff(levels(as.factor(test_data$def_naturaleza)), "Violencia interpersonal"),
                                       "Violencia interpersonal"))
y <- as.numeric(as.factor(test_data$def_naturaleza))
N <- nrow(X)
P <- ncol(X)
K <- length(unique(y))

# nimble model" 
code <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dcat(prob[i, 1:K])
    for (k in 1:(K - 1)) {  # El último nivel es la categoría de referencia
      logit_phi[i, k] <- inprod(beta[k, 1:P], X[i, 1:P])
    }
    
    # Softmax normalizado (última clase es referencia)
    for (k in 1:(K - 1)) {
      exp_phi[i, k] <- exp(logit_phi[i, k])
    }
    exp_phi[i, K] <- 1
    
    for (k in 1:K) {
      prob[i, k] <- exp_phi[i, k] / sum(exp_phi[i, 1:K])
    }
  }
  
  # Priors
  for (k in 1:(K - 1)) {
    for (j in 1:P) {
      beta[k, j] ~ dnorm(0, sd = 5)  # Priors para betas
    }
  }
})

constants <- list(N = N, P = P, K = K, X = X)
data <- list(y = y)
inits <- list(beta = matrix(0, nrow = K - 1, ncol = P))

model <- nimbleModel(code, data = data, inits = inits, constants = constants)
cmodel <- compileNimble(model)

conf <- configureMCMC(model, monitors = c("beta"))
mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = model)

samples <- runMCMC(cmcmc, niter = 2000, nburnin = 1000, nchains = 1, thin = 1, samplesAsCodaMCMC = TRUE)
