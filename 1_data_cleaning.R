#' This script is coppied and adapted from the
#' "Situation Dependent Learning" project and its
#' Analysis GitHub repo at commit 5c8e0a6.

library(vroom)
library(tidyverse)


study_stage <- 'param_recovery'  # With what part of the study are we dealing here?
raw_dat_path <- file.path('..', 'data', 'raw')
clean_dat_path <- file.path('..', 'data', 'clean')

all_dat <- vroom(file.path(raw_dat_path, list.files(raw_dat_path)))


# Reshaping ---------------------------------------------------------------
dat_main <- all_dat %>%
  dplyr::select(starts_with(c("participant.code", "Investment_Task.")) &
    -contains(c('round_number', 'id_in_group')))

names(dat_main) <- str_remove_all(names(dat_main),
  'Investment_Task.|subsession.|player.|group.')

dat_main <- rename(dat_main, participant_code = participant.code)

inv_task_dat_long <- dat_main %>%
  pivot_longer(cols = -participant_code,
    names_to = c("round_number", ".value"),
    names_sep = "\\.")

# "Filling up" the last round:
inv_task_dat_long$i_round_in_block <- replace_na(inv_task_dat_long$i_round_in_block, 75)
inv_task_dat_long$hold[inv_task_dat_long$i_round_in_block == 75] <-
with(inv_task_dat_long,
  hold[i_round_in_block == 74] + transaction[i_round_in_block == 74])

# Adding the other variables -----------------------------------------------
other_dat <- all_dat %>%
  dplyr::select((starts_with('Demographics') |
    contains(c('participant.code', 'time_started',
    'cogn_rpm_total_points', '_soep_', 'participant.payoff',
    'ambiguity_aversion', 'loss_aversion')) |
    'session.code') &
    -contains(c('config', 'Tutorial_', 'group',
      'Demographics.1.player.payoff', 'Demographics.1.player.round_number',
      'Demographics.1.subsession.round_number',
      'SOEP5.1.subsession.round_number')))

other_dat <- rename(other_dat,
  participant_code = participant.code,
  session_code = session.code,
  participant_payoff = participant.payoff)

names(other_dat) <- str_split(names(other_dat), '\\.') %>%
  lapply(tail, 1) %>%
  unlist()

dat_main_long <- left_join(inv_task_dat_long, other_dat,
  by = 'participant_code')


# Save Data ---------------------------------------------------------------

# Re-save that dataframe with the additional variables
# TODO: general comments are saved incorrectly! Fix that!
write_delim(dat_main_long, file.path(clean_dat_path,
    str_c('all_participants_long_main_', study_stage, '.csv')),
            delim = ';')

# Loading the data in case it gets lost again:
# dat_main_long <- read_delim('..//Data//Clean//all_participants_long_main_main_study.csv', delim = ';')
