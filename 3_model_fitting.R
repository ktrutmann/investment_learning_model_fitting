	library(rstan)
	library(tidyverse)

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

dat_main_long <- read_delim(file.path('..', 'data', 'input',
	'all_participants_long_main_param_recovery.csv'), delim = ';')


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
# Starting with only one participant and two blocks for testing purposes
fit_dat <- filter(dat_main_long,
	i_block <= 1, i_round_in_block != 75,
	participant_code %in% unique(participant_code)[1:10])

# Data for Stan (names must correspond to that in .stan file):
# Note: + 0 is used to convert the boolean to integer matrix
stan_dat <- list(
	dat_len = max(table(fit_dat$participant_code)),
	n_subj = length(unique(fit_dat$participant_code)),
	round_in_block = make_stan_matrix(fit_dat, 'i_round_in_block'),
	belief = make_stan_matrix(fit_dat, 'belief') / 100,
	hold_lead = make_stan_matrix(fit_dat, 'hold_lead') + 2,
	invested = (make_stan_matrix(fit_dat, 'hold') != 0) + 0,
	gain_position = (make_stan_matrix(fit_dat, 'returns') > 0) + 0,
	loss_position = (make_stan_matrix(fit_dat, 'returns') < 0) + 0,
	favorable_move = (make_stan_matrix(fit_dat,
		'price_move_from_last_corrected') == 'Favorable') + 0,
	current_price = (make_stan_matrix(fit_dat, 'price')),
	bayes_probs = make_stan_matrix(fit_dat, 'bayes_prob_up')
)

# Fitting ----------------------------------------------------
fit_bayesian_updater <- stan(
	file = file.path('models', 'multi_alpha_rl.stan'),
	data = stan_dat,
	iter = 12000,
	warmup = 2000,
	chains = 1,
	cores = 4)

saveRDS(fit_bayesian_updater, file.path('saved_objects', 'fit_bayesian_updater.RDS'))
