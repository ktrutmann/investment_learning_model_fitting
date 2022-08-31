library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat_main_long <- read_delim(file.path('..', 'data', 'clean',
	'all_participants_long_main_study.csv'), delim = ';',
	guess_max = 5000)


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
	filter(i_block <= 1, i_round_in_block != 75,
#	       TRUE) %>%
#	       !participant_code %in% unique(participant_code)[c(103)]) %>% # CSRL 5k
#	       !participant_code %in% unique(participant_code)[c(90)]) %>% # sinlge alpha 5k
	       !participant_code %in% unique(participant_code)[c(90, 103)]) %>% # All 20k
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
starttime <- format(Sys.time(), '%y%m%d-%H00')
name_this_run <- 'CSRL_25k_no90_103'
fitted_model <- stan(
	file = file.path('models', 'multi_alpha_rl.stan'),
	data = stan_dat,
	iter = 21000,
#	warmup = 500, # Used for the 5k fitting
	warmup = 1000, # Used for the 20k fitting
	chains = 4,
	cores = 4,
	init = rep(list(list(alphas_raw = matrix(0, nrow = stan_dat$n_subj, ncol = 5),
			 hyper_alphas = rep(1,5))), 4),
#	init = rep(list(list(alpha_raw = rep(03 stan_dat$n_subj),
#			 hyper_alpha = 1)), 4),
	save_warmup = FALSE,
	refresh = 100,
	sample_file = file.path('..', 'data', 'saved_objects', 'sampling_files',
	str_c('samples_', starttime, '_', name_this_run, '.csv')),
	seed = 112358)

saveRDS(fitted_model, file.path('..', 'data', 'saved_objects',
	str_c(starttime, '_', name_this_run, '.RDS')))

