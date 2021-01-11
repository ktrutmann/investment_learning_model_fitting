data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=0, upper=1> invested[dat_len, n_subj]; // Invest or short?
  int<lower=0, upper=1> gain_position[dat_len, n_subj];
  int<lower=0, upper=1> loss_position[dat_len, n_subj];
  int<lower=0, upper=1> favorable_move[dat_len, n_subj];
}


parameters{
  real hyper_alpha; // learning rate hyperparameters
  real <lower=0> hyper_alpha_sd; // Learning rate standard deviation
  real alpha_raw[n_subj]; // The individual learning rate

  real<lower=0> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> hyper_sigma_sd; // Hyperparameter for the reporting error
  real<lower=0> sigmas_raw[n_subj];  // "Reporting error variance" parameter
}


transformed parameters{
  real  alpha[n_subj];
  real  sigma[n_subj];
  // Non-centered parameterisation
  for (i in 1:n_subj){
    alpha[i] = Phi(hyper_alpha + hyper_alpha_sd * alpha_raw[i]);
    sigma[i] = hyper_sigma + hyper_sigma_sd * sigmas_raw[i];
  }
}

model{
  // Priors following Fontanesi19

  // Hyperpriors
  hyper_alpha ~ normal(-.5, .5);
  hyper_alpha_sd ~ gamma(1.2, 3);

  hyper_sigma ~ gamma(1.2, 3);
  hyper_sigma_sd ~ gamma(1.2, 3);

  for (i_subj in 1:n_subj){
    real model_belief;

    // individual priors
    alpha_raw[i_subj] ~ std_normal();
    sigmas_raw[i_subj] ~ std_normal();

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {
        model_belief = model_belief + alpha[i_subj] *
          (favorable_move[i_trial, i_subj] - model_belief);
      }
      // TODO: (5) Put the model loop into the transformed params. section?
      belief[i_trial, i_subj] ~ normal(model_belief, sigma[i_subj]) T[0, 1];
    }
  }
}
