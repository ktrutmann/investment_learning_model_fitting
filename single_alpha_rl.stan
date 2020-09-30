data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  // TODO: Check out whether it's faster to switch the matrix into a vector of vectors
  int<lower=-1, upper=1> invested[dat_len, n_subj]; // Invest or short?
  int<lower=0, upper=1> gain_position[dat_len, n_subj];
  int<lower=0, upper=1> loss_position[dat_len, n_subj];
  int<lower=0, upper=1> price_increase[dat_len, n_subj];
}


parameters{
  real<lower=0, upper=1> hyper_alpha; // Hyperparameter for the learning rate
  real<lower=0, upper=1> alpha[n_subj];  // The learning rate
  // This will have to be a vector and there will need to be a hyperparameters
  real<lower=0, upper=1> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> sigma[n_subj];  // "Reporting error variance" parameter
}


model{
  // Priors
  hyper_alpha ~ normal(.2, .5);  // Truncated normal around "rational" value
  hyper_alpha_sigma ~ gamma(1.2, 1.2);
  hyper_sigma ~ gamma(1.2, 1.2);
  // TODO: Add hyper_sigma_sigma


  for (i_subj in 1:n_subj) {
    real model_belief;  // The belief the model would hold
    
    alpha ~ normal(hyper_alpha, hyper_alpha_sigma);
    sigma ~ normal(hyper_sigma, .5);  // "Reporting error variance"

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {

        model_belief = .5;

      } else {

        model_belief += model_belief +
          alpha[i_subj] * (price_increase[i_trial, i_subj] -
            model_belief);
      }
    }

    target += normal_lpdf(belief[:, i_subj] | model_belief, sigma[i_subj]);
  }
}


generated quantities {
  matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

  for (i_subj in 1:n_subj){
    real model_belief;  // The belief the model would hold

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {

        model_belief = .5;

      } else {

        model_belief += model_belief +
          alpha[i_subj] * (price_increase[i_trial, i_subj] -
            model_belief);
      }

      log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma[i_subj]);
    }
  }
}
