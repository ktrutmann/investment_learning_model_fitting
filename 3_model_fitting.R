library(rstan)
library(tidyverse)
source('notifier.R')

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

dat_main_long <- read_delim(file.path('..', 'data', 'clean',
	'all_participants_long_main_main_study.csv'), delim = ';')

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
fit_dat <- filter(dat_main_long,
	i_block <= 1, i_round_in_block != 75)

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
	up_move = (make_stan_matrix(fit_dat,
		'price_diff_from_last') > 0) + 0,
	current_price = (make_stan_matrix(fit_dat, 'price')),
	bayes_probs = make_stan_matrix(fit_dat, 'bayes_prob_up')
)

stan_dat$up_move[1, 1] <- 0

# Fitting ----------------------------------------------------
fitted_model_rl_plus <- stan(
	file = file.path('models', 'multi_alpha_rl.stan'),
	data = stan_dat,
	iter = 21000,
	warmup = 1000,
	chains = 4,
	cores = 4,
	save_warmup = FALSE,
	refresh = 100,
	pars = c('hyper_alphas',
			 'hyper_alpha_sds',
			 'hyper_sigma',
			 'hyper_sigma_sd'),
	sample_file = file.path('..', 'saved_objects',
		str_c(format(Sys.time(), "%y%m%d"),
			'_samples_rl_plus_main_study.csv')))

saveRDS(fitted_model_rl_plus, file.path('..', 'saved_objects',
	str_c(format(Sys.time(), "%y%m%d"), '_rl_plus_main_study.RDS')))

notify_me('RL Plus')

rm(fitted_model_rl_plus)

fitted_model_rl_simple <- stan(
	file = file.path('models', 'single_alpha_rl.stan'),
	data = stan_dat,
	iter = 21000,
	warmup = 1000,
	chains = 4,
	cores = 4,
	save_warmup = FALSE,
	refresh = 100,
	pars = c('hyper_alpha',
			 'hyper_alpha_sd',
			 'hyper_sigma',
			 'hyper_sigma_sd'),
	sample_file = file.path('..', 'saved_objects',
		str_c(format(Sys.time(), "%y%m%d"),
			'_samples_rl_simple_main_study.csv')))

saveRDS(fitted_model_rl_simple, file.path('..', 'saved_objects',
	str_c(format(Sys.time(), "%y%m%d"), '_rl_simple_main_study.RDS')))

notify_me('RL Simple')