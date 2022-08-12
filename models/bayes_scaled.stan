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
  vector<lower=0> [n_subj] sigma_raw;  // "Reporting error variance" parameter
}

transformed parameters{
  vector<lower=0> [n_subj] scaling_param;
  vector<lower=.02> [n_subj] sigma;

  scaling_param = hyper_scaling_param + hyper_scaling_param_sd * scaling_param_raw;
  // Truncating sigma at .02 because otherwise subj 111 doesn't converge!
  // This seems to be slightly faster than truncating the parameter directly.
  sigma = sigma_raw + .02;
}

model{
  // Priors
  scaling_param_raw ~ std_normal();
  hyper_sigma_shape ~ gamma(10, .3);
  hyper_sigma_rate ~ gamma(15, .2);

  for (i_subj in 1:n_subj) {
    sigma_raw[i_subj] ~ gamma(hyper_sigma_shape, hyper_sigma_rate);

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
