library(rstan)
library(loo)
library(shinystan)
library(bayesplot)
library(tidybayes)
library(plotly)
library(tidyverse)

dat_main_long <- read_delim(file.path('..', 'Data',
	'Clean', 'param_recovery', 'all_participants_long_main_param_recov.csv'),
	delim = ';')

# Helper functions -----------------------------------------------

#' This creates a matrix with one row per period and one column per subject.
#' df: The dataframe from which to extract the variables
#' content_var: What variable to fill the matrix with
make_stan_matrix <- function(df, content_var) {
	print(stringr::str_c('Making matrix for ', content_var))

	stan_mat <- df %>%
		dplyr::select('round_number',
			'participant_code', all_of(content_var)) %>%
		pivot_wider(names_from = participant_code,
			values_from = content_var) %>%
		select(-'round_number') %>%
		as.matrix()
		# FIXME: tidyr is still not happy with how I select the variables...

	return(stan_mat)
}

# Getting Data ready --------------------------------------------------------
# Starting with only one participant and one block for testing purposes
fit_dat <- filter(dat_main_long, i_block <= 1, !is.na(belief))

# Data for Stan (names must correspond to that in .stan file):
# Note: + 0 is used to convert the boolean to integer matrix
stan_dat <- list(
	dat_len = table(fit_dat$participant_code)[1],
	n_subj = length(unique(fit_dat$participant_code)),
	round_in_block = make_stan_matrix(fit_dat, 'i_round_in_block'), 
	belief = make_stan_matrix(fit_dat, 'belief') / 100,
	invested = (make_stan_matrix(fit_dat, 'hold') != 0) + 0,
	gain_position = (make_stan_matrix(fit_dat, 'returns') > 0) + 0,
	loss_position = (make_stan_matrix(fit_dat, 'returns') < 0) + 0,
	price_increase = (make_stan_matrix(fit_dat,
		'price_diff_from_last') > 0) + 0,
	bayes_probs = make_stan_matrix(fit_dat, 'bayes_prob_up')
)

# Fitting ----------------------------------------------------
fit_bayesian_updater <- stan(file = 'bayes_updater.stan',
	data = stan_dat,
	iter = 8000,
	warmup = 2000,
	cores = 4)

saveRDS(fit_bayesian_updater, file.path('saved_objects', 'fit_bayesian_updater.RDS'))


fit_one_alpha <- stan(file = 'single_alpha_rl.stan',
	data = stan_dat,
	iter = 8000,
	warmup = 2000,
	cores = 4)

saveRDS(fit_one_alpha, file.path('saved_objects', 'fit_one_alpha.RDS'))


fit_2_alphas <- stan(file = 'multi_alpha_rl.stan',
	data = stan_dat,
	iter = 8000,
	warmup = 2000,
	cores = 4)

saveRDS(fit_2_alphas, file.path('saved_objects', 'fit_2_alphas.RDS'))

# TODO: Fit the model to all baseline-blocks, then cross-validate
# TODO: Generalize by fitting to block rand(c(1,2)) and then evaluate on the other block