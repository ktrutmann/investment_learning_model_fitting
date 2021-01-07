library(loo)
library(plotly)
library(shinystan)
library(bayesplot)
library(tidybayes)

fitted_model <- readRDS(file.path('saved_objects', 'rl_plus_main_study.RDS'))

# Plot Diagnostics -----------------------------------------------

pairs(fitted_model, pars = names(fitted_model@sim$samples[[1]])[c(1,2,6,7,11,12, 36, 37, 38)])

model_trace <- rstan::extract(fitted_model,
	inc_warmup = TRUE, permuted = FALSE)

mcmc_dens(model_trace, pars = vars(contains('transf_hyper_alpha')))

qplot(model_trace[, , 'transf_hyper_alpha[3]'] -
	model_trace[, , 'transf_hyper_alpha[1]'])

# LOO evaluation ----------------------------------------------------
log_lik_bayes_updater <- extract_log_lik(fit_bayesian_updater,
	merge_chains = FALSE)

r_eff_bayes_updater <- relative_eff(exp(log_lik_bayes_updater), cores = 4)

loo_bayes_updater <- loo::loo(log_lik_bayes_updater,
	r_eff = r_eff_bayes_updater, cores = 4)
saveRDS(loo_bayes_updater, file.path('saved_objects', 'loo_bayes_updater.RDS'))
print(loo_bayes_updater)


loo_2_alphas <- loo::loo(log_lik_2_alphas, r_eff = r_eff_2_alphas, cores = 4)
saveRDS(loo_2_alphas, file.path('saved_objects', 'loo_2_alphas.RDS'))
print(loo_2_alphas)

loo_compare(loo_bayes_updater, loo_one_alpha, loo_2_alphas)
