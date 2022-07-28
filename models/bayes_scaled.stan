// This model takes the Bayesian probability of an upward move and scales it around .5.
// This means that it doesn't scale the belief itself, but the distance to the "neutral" .5.

data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  matrix<lower=0, upper=1>[dat_len, n_subj] bayes_probs; // The objective bayesian probability
}


parameters{
  real<lower=0> hyper_scaling_param; // Scaling factor
  real<lower=0> hyper_scaling_param_sd; // Scaling factor sd
  vector<lower=0> [n_subj] scaling_param_raw;  // Individual Scaling factor

  real<lower=0> hyper_sigma_shape; // Hyperparameter for the reporting error shape
  real<lower=0> hyper_sigma_rate; // Hyperparameter for the reporting error rate
  // TODO: (1) Check whether the restriction of .02 is necessary here as well!
  vector<lower=0> [n_subj] sigma;  // "Reporting error variance" parameter
}

transformed parameters{
  vector<lower=0> [n_subj] scaling_param;
  scaling_param = hyper_scaling_param + hyper_scaling_param_sd * scaling_param_raw;
}

model{
  // Priors
  hyper_scaling_param ~ gamma(10, 5);
  hyper_scaling_param_sd ~ gamma(10, 5);
  scaling_param_raw ~ std_normal();

  hyper_sigma_shape ~ normal(30, 10);
  hyper_sigma_rate ~ normal(80, 20);
  sigma ~ gamma(hyper_sigma_shape, hyper_sigma_rate);


  for (i_subj in 1:n_subj) {
    target += normal_lpdf(belief[:, i_subj] |
      (bayes_probs[:, i_subj] - .5) * scaling_param[i_subj] + .5, sigma[i_subj]);
  }
}


// generated quantities {
//   matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

//   for (i_subj in 1:n_subj){
//     for (i_trial in 1:dat_len) {
//       log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
//         bayes_probs[i_trial, i_subj], sigma[i_subj]);
//     }
//   }
// }
