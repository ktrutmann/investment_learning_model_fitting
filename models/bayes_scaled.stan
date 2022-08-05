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

  real<lower=0> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> hyper_sigma_sd; // Hyperparameter for the reporting error
  vector<lower=0> [n_subj] sigmas_raw;  // "Reporting error variance" parameter
}

transformed parameters{
  vector<lower=0> [n_subj] scaling_param;
  vector<lower=.02> [n_subj] sigma;

  scaling_param = hyper_scaling_param + hyper_scaling_param_sd * scaling_param_raw;

  sigma = hyper_sigma + hyper_sigma_sd * sigmas_raw;
  for (i in 1:n_subj) {
    sigma[i] = max([.02, sigma[i]]');
  }
}

model{
  // Priors
  hyper_sigma ~ gamma(5, 10);
  hyper_sigma_sd ~ gamma(5, 10);
  sigmas_raw ~ std_normal();
  scaling_param_raw ~ std_normal();

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
