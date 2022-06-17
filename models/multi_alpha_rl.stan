functions{
  real update_model_belief(real prev_belief, int last_transaction, int gain_pos,
                      int loss_pos, int favorable_move, int up_move,
                      row_vector alphas) {

    if (last_transaction != 0 || (gain_pos == 0 && loss_pos == 0)) {
      return prev_belief + alphas[1] * (up_move - prev_belief);
    }
    if ((gain_pos == 1) && (favorable_move == 1)) {
      return prev_belief + alphas[2] * (up_move - prev_belief);
    }
    if ((gain_pos == 1) && (favorable_move == 0)) {
      return prev_belief + alphas[3] * (up_move - prev_belief);
    }
    if ((loss_pos == 1) && (favorable_move == 1)) {
      return prev_belief + alphas[4] * (up_move - prev_belief);
    }
    if ((loss_pos == 1) && (favorable_move == 0)) {
      return prev_belief + alphas[5] * (up_move - prev_belief);
    }
    return -999;  // If this happens, it should raise some red flags
  }
}


data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=-2, upper=2> last_transaction[dat_len, n_subj]; // Invest or short?
  int<lower=0, upper=1> gain_position[dat_len, n_subj];
  int<lower=0, upper=1> loss_position[dat_len, n_subj];
  int<lower=0, upper=1> up_move[dat_len, n_subj];
  int<lower=0, upper=1> favorable_move[dat_len, n_subj];
}


parameters{
  vector [5] hyper_alphas; // learning rate hyperparameters
  vector <lower=0> [5] hyper_alpha_sds; // Learning rate standard deviation
  matrix [n_subj, 5] alphas_raw; // The individual learning rate

  real<lower=0> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> hyper_sigma_sd; // Hyperparameter for the reporting error
  vector<lower=0> [n_subj] sigmas_raw;  // "Reporting error variance" parameter
}


transformed parameters{
  matrix [n_subj, 5] alphas;
  vector [n_subj] sigma;
  // Non-centered parameterisation
  for (i in 1:5){
    alphas[:, i] = Phi(hyper_alphas[i] + hyper_alpha_sds[i] * alphas_raw[:, i]);
  }
  sigma = hyper_sigma + hyper_sigma_sd * sigmas_raw;
}

model{

  // Priors following Fontanesi19
  // Hyperpriors
  for (i in 1:5){
    hyper_alphas[i] ~ normal(-.5, .5);
    hyper_alpha_sds[i] ~ gamma(1.2, 3);
  }

  hyper_sigma ~ gamma(1.2, 3);
  hyper_sigma_sd ~ gamma(1.2, 3);

  
  for (i_subj in 1:n_subj){
    real model_belief;
    
    // individual priors
    for (i in 1:5){
      alphas_raw[i_subj, :] ~ std_normal();
    }
    sigmas_raw[i_subj] ~ std_normal();

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {

        model_belief = update_model_belief(
          model_belief,
          last_transaction[i_trial, i_subj],
          gain_position[i_trial, i_subj],
          loss_position[i_trial, i_subj],
          favorable_move[i_trial, i_subj],
          up_move[i_trial, i_subj],
          alphas[i_subj, :]);
      }
      // Normal truncated by [0, 1]
      target += normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma[i_subj]) -
        log_diff_exp(normal_lcdf(1 | model_belief, sigma[i_subj]),
                            normal_lcdf(0 | model_belief, sigma[i_subj]));
    }
  }
}


generated quantities {
  matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

  for (i_subj in 1:n_subj){
    real model_belief;

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {
        model_belief = update_model_belief(
          model_belief,
          last_transaction[i_trial, i_subj],
          gain_position[i_trial, i_subj],
          loss_position[i_trial, i_subj],
          favorable_move[i_trial, i_subj],
          up_move[i_trial, i_subj],
          alphas[i_subj, :]);
      }

    log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma[i_subj]) -
        log_diff_exp(normal_lcdf(1 | model_belief, sigma[i_subj]),
                            normal_lcdf(0 | model_belief, sigma[i_subj]));
    }
  }
}
