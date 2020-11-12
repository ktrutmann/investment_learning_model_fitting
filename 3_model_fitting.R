	library(rstan)
	library(loo)
	library(plotly)
	library(shinystan)
	library(bayesplot)
	library(tidybayes)
	library(tidyverse)

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

dat_main_long <- read_delim(file.path('..', 'data', 'clean',
	'param_recovery', 'all_participants_long_main_param_recovery.csv'),
	delim = ';')

# Helper functions -----------------------------------------------

#' This creates a matrix with one row per period and one column per subject.
#' df: The dataframe from which to extract the variables
#' content_var: What variable to fill the matrix with
make_stan_matrix <- function(df, content_var) {
	cat(stringr::str_c('Making matrix for ', content_var, '\n'))

	stan_mat <- df %>%
		dplyr::select('round_number',
			'participant_code', all_of(content_var)) %>%
		pivot_wider(names_from = 'participant_code',
			values_from = all_of(content_var)) %>%
		select(-'round_number') %>%
		as.matrix()
	return(stan_mat)
}

# Getting Data ready --------------------------------------------------------
# Starting with only one participant and one block for testing purposes
# TODO: (6) rm the one participant filter 
fit_dat <- filter(dat_main_long, i_block <= 1, !is.na(belief),
	participant_code == unique(participant_code)[1])

# Data for Stan (names must correspond to that in .stan file):
# Note: + 0 is used to convert the boolean to integer matrix
# TODO: (2) Make sure we use "hold_lead" to fit on the decision not the current portfolio
stan_dat <- list(
	dat_len = table(fit_dat$participant_code)[1],
	n_subj = length(unique(fit_dat$participant_code)),
	round_in_block = make_stan_matrix(fit_dat, 'i_round_in_block'),
	belief = make_stan_matrix(fit_dat, 'belief') / 100,
	hold = make_stan_matrix(fit_dat, 'hold') + 2,
	invested = (make_stan_matrix(fit_dat, 'hold') != 0) + 0,
	gain_position = (make_stan_matrix(fit_dat, 'returns') > 0) + 0,
	loss_position = (make_stan_matrix(fit_dat, 'returns') < 0) + 0,
	price_increase = (make_stan_matrix(fit_dat,
		'price_diff_from_last') > 0) + 0,
	current_price = (make_stan_matrix(fit_dat, 'price')),
	bayes_probs = make_stan_matrix(fit_dat, 'bayes_prob_up')
)

# Fitting ----------------------------------------------------
fit_bayesian_updater <- stan(file = 'bayes_EU.stan',
	data = stan_dat,
	iter = 12000,
	warmup = 2000,
	cores = 4)

saveRDS(fit_bayesian_updater, file.path('saved_objects', 'fit_bayesian_updater.RDS'))


fit_one_alpha <- stan(file = 'single_alpha_rl.stan',
	data = stan_dat,
	iter = 12000,
	warmup = 2000,
	cores = 4)

saveRDS(fit_one_alpha, file.path('saved_objects', 'fit_one_alpha.RDS'))


fit_2_alphas <- stan(file = 'multi_alpha_rl.stan',
	data = stan_dat,
	iter = 12000,
	warmup = 2000,
	cores = 4)

saveRDS(fit_2_alphas, file.path('saved_objects', 'fit_2_alphas.RDS')) 