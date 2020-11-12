data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  matrix<lower=0, upper=1>[dat_len, n_subj] bayes_probs; // The objective bayesian probability
}


parameters{
  real<lower=0, upper=1> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> sigma[n_subj];  // "Reporting error variance" parameter
}


model{
  // Priors
  hyper_sigma ~ gamma(1.2, 1.2);

  for (i_subj in 1:n_subj) {
    target += normal_lpdf(belief[:, i_subj] |
      bayes_probs[:, i_subj], sigma[i_subj]);
  }
}


generated quantities {
  matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

  for (i_subj in 1:n_subj){
    for (i_trial in 1:dat_len) {
      log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
        bayes_probs[i_trial, i_subj], sigma[i_subj]);
    }
  }
}
