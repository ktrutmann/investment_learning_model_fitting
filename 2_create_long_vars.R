#' This script is coppied and adapted from the
#' "Situation Dependent Learning" project and its
#' Analysis GitHub repo at commit 5c8e0a6.

# Loading Data: ################################################################

study_stage <- 'param_recovery'  # With what part of the study are we dealing here?
clean_dat_path <- file.path('..', 'data', 'clean')

dat_main_long <- read_delim(file.path(clean_dat_path,
  str_c('all_participants_long_main_', study_stage, '.csv')),
delim = ';')


# Constants ###############################################################
seed <- 1234

# Creating Variables ######################################################
# A function to generate a "cumulative amount of price moves while invested"
count_n_moves <- function(moves, invested) {
  n_moves_vec <- vector(mode = 'numeric', length = length(moves))
  n_moves_now <- 0
  for (i in seq_along(moves)) {
    if (moves[i] == 'Undefined' | is.na(moves[i])
      | is.na(invested[i])) {  # Guard against NAs
      n_moves_vec[i] <- NA
      n_moves_now <- 0
      next
    }
    # Register "switches" as restarts:
    if (!is.na(invested[i - 1]) & invested[i - 1] != invested[i])
      n_moves_now <- 0

    if (invested[i] != 0) {
      if (moves[i] == 'Favorable') {
        n_moves_now <- n_moves_now + 1
      } else {
        n_moves_now <- n_moves_now - 1
      }
      n_moves_vec[i] <- n_moves_now
    } else {
      n_moves_now <- 0
      n_moves_vec[i] <- NA
    }
  }
  return(n_moves_vec)
}

# Creating the variables-------------------------------------------------------
dat_main_long <- dat_main_long %>%
  mutate(participant_code = as.factor(participant_code),
   belief_diff_from_last = c(NA, diff(belief)),
   rational_belief = case_when(
    condition == 'states_shown' & state == 1 ~ .65,
    condition == 'states_shown' & state == 0 ~ .35,
    TRUE ~ bayes_prob_up),
   bayes_diff_from_last = c(NA, diff(rational_belief)),
   belief_diff_bayes_corrected = belief_diff_from_last -
   (bayes_diff_from_last * 100),
   updating_type = case_when(sign(belief_diff_from_last) !=
    sign(bayes_diff_from_last) ~ 'Wrong',
    abs(belief_diff_from_last) > abs(bayes_diff_from_last * 100)
    ~ 'Over', TRUE ~ 'Under'),
   price_diff_from_last = c(NA, diff(price)),
   price_diff_to_next = c(diff(price), NA),
   price_move_from_last = as.factor(c(NA, if_else(diff(price) > 0,
    'Up', 'Down'))),
   price_move_from_last_corrected = as.factor(
    case_when(hold == -1 & price_diff_from_last < 0 ~ 'Favorable',
      hold == -1 & price_diff_from_last > 0 ~ 'Unfavorable',
      hold != -1 & price_diff_from_last < 0 ~ 'Unfavorable',
      hold != -1 & price_diff_from_last > 0 ~ 'Favorable',
      TRUE ~ 'Undefined')),
   position = case_when(
      hold != 0 & returns > 0 ~ 'Gain',
      hold != 0 & returns < 0 ~ 'Loss',
      hold != 0 & returns == 0 ~ 'No Returns',
      TRUE ~ 'Not Invested'),
   position_end_of_last_period = as.factor(dplyr::lag(case_when(
      position == 'Not Invested' & abs(transaction) == 1 ~ 'No Returns',
      position != 'Not Invested' & abs(transaction) == 1 ~ 'Not Invested',
      abs(transaction) == 2 ~ 'No Returns',
      TRUE ~ position))),
   cumulative_moves_while_invested = count_n_moves(
    price_move_from_last_corrected, hold),
   belief_diff_flipped = if_else(price_diff_from_last > 0,
    belief_diff_from_last, - belief_diff_from_last),
   belief_diff_bayes_corrected_flipped = if_else(
    price_diff_from_last > 0,
    belief_diff_bayes_corrected, - belief_diff_bayes_corrected)
  )

  dat_main_long$hold_lead <- lead(dat_main_long$hold)
  dat_main_long$hold_lead[dat_main_long$i_round_in_block == 75] <- NA

# "Optimal Trading" -----------------------------------------------------------
dat_main_long$rational_hold <- if_else(
  dplyr::lag(dat_main_long$bayes_prob_up) >= .5, 1, -1)
dat_main_long$rational_hold <- if_else(lead(dat_main_long$condition) == 'states_shown',
  if_else(dplyr::lag(dat_main_long$state) == 1, 1, -1), dat_main_long$rational_hold)

dat_main_long$rational_trading_earnings <- replace_na(
  dat_main_long$price_diff_from_last * dat_main_long$rational_hold, 0)

set.seed(seed = seed)
lottery_probs <- runif(nrow(dat_main_long))
# Whether the lottery is won if played:
lottery_wins <- sapply(lottery_probs,
  function(x) sample(c(TRUE, FALSE), 1, prob = c(x, 1 - x)))

dat_main_long$rational_lottery_earnings <-
  as.integer(if_else(lottery_probs < dat_main_long$rational_belief,
    lottery_wins,
    dat_main_long$price_diff_to_next > 0)) %>%
  replace_na(0)

dat_main_long$rational_complete_earnings <- dat_main_long$rational_trading_earnings +
  dat_main_long$rational_lottery_earnings

# Save Data ---------------------------------------------------------------

# Re-save that dataframe with the additional variables
# TODO: general comments are saved incorrectly! Fix that!
write_delim(dat_main_long, file.path(clean_dat_path,
  str_c('all_participants_long_main_', study_stage, '.csv')),
delim = ';')

# Loading the data in case it gets lost again:
# dat_main_long <- read_delim('..//Data//Clean//all_participants_long_main_main_study.csv', delim = ';')
