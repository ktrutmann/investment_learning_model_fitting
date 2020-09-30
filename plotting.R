mcmc_intervals(fit_bayesian_updater,
			   pars = vars(contains('hyper')),
			   prob = 0.9,
			   prob_outer = .95)

mcmc_trace(fit_bayesian_updater,
			   pars = vars(contains('hyper')))