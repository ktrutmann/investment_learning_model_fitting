# LOO evaluation ----------------------------------------------------
log_lik_bayes_updater <- extract_log_lik(fit_bayesian_updater,
	merge_chains = FALSE)

r_eff_bayes_updater <- relative_eff(exp(log_lik_bayes_updater), cores = 4)

loo_bayes_updater <- loo::loo(log_lik_bayes_updater,
	r_eff = r_eff_bayes_updater, cores = 4)
saveRDS(loo_bayes_updater, file.path('saved_objects', 'loo_bayes_updater.RDS'))
print(loo_bayes_updater)


log_lik_one_alpha <- extract_log_lik(fit_one_alpha, merge_chains = FALSE)

r_eff_one_alpha <- relative_eff(exp(log_lik_one_alpha), cores = 4)

loo_one_alpha <- loo::loo(log_lik_one_alpha,
	r_eff = r_eff_one_alpha, cores = 4)
saveRDS(loo_one_alpha, file.path('saved_objects', 'loo_one_alpha.RDS'))
print(loo_one_alpha)


log_lik_2_alphas <- extract_log_lik(fit_2_alphas, merge_chains = FALSE)

r_eff_2_alphas <- relative_eff(exp(log_lik_2_alphas), cores = 4)

loo_2_alphas <- loo::loo(log_lik_2_alphas, r_eff = r_eff_2_alphas, cores = 4)
saveRDS(loo_2_alphas, file.path('saved_objects', 'loo_2_alphas.RDS'))
print(loo_2_alphas)


loo_compare(loo_bayes_updater, loo_one_alpha, loo_2_alphas)

# Conclusion: For simulation: Two alphas is best, but scaled bayesian updater is better than one alpha... whut?
# TODO: Check if one-alpha RL worked as intended!

# TODO: Program the split sample cross validation
