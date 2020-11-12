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
