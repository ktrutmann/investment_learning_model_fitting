library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

dat_main_long <- read_delim(file.path('..', 'data', 'clean',
	'all_participants_long_main_param_recov.csv'), delim = ';')

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
fit_dat <- dat_main_long %>%
	mutate(last_transaction = lag(transaction),
		last_returns = lag(returns)) %>%
	filter(i_block <= 1, i_round_in_block != 75) %>%
	mutate(updated_from_code = case_when(
		is.na(updated_from) ~ 1,
		updated_from == 'Not Invested' ~ 1,
		updated_from == 'Favorable Gain' ~ 2,
		updated_from == 'Unfavorable Gain' ~ 3,
		updated_from == 'Favorable Loss' ~ 4,
		updated_from == 'Unfavorable Loss' ~ 5
	))

# Data for Stan (names must correspond to that in .stan file):
# Note: + 0 is used to convert the boolean to integer matrix
stan_dat <- list(
	dat_len = max(table(fit_dat$participant_code)),
	n_subj = length(unique(fit_dat$participant_code)),
	round_in_block = make_stan_matrix(fit_dat, 'i_round_in_block'),
	belief = make_stan_matrix(fit_dat, 'belief') / 100,
	updating_from = make_stan_matrix(fit_dat, 'updated_from_code'),
	up_move = (make_stan_matrix(fit_dat,
		'price_diff_from_last') > 0) + 0,
	current_price = (make_stan_matrix(fit_dat, 'price')),
	bayes_probs = make_stan_matrix(fit_dat, 'bayes_prob_up')
)

stan_dat$up_move[1, 1] <- 0


# Fitting ----------------------------------------------------
fitted_model_rl_inv <- stan(
	file = file.path('models', 'multi_alpha_rl.stan'),
	data = stan_dat,
	iter = 21000,
	warmup = 1000,
	chains = 4,
	cores = 4,
	save_warmup = FALSE,
	refresh = 100,
	pars = c('hyper_alphas',
			 'alphas'),
	sample_file = file.path('..', 'data', 'saved_objects',
		str_c(format(Sys.time(), '%y%m%d-%H00'),
			'_samples_rl_plus_param_recov.csv')))

saveRDS(fitted_model_rl_inv, file.path('..', 'data', 'saved_objects',
	str_c(format(Sys.time(), '%y%m%d-%H00'), '_rl_plus_param_recov.RDS')))
