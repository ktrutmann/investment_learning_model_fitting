// This file contains the single alpha model with hierarchical
// (and non-centered paramerized) alphas and (reporting error) for all participants.

data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=0, upper=1> up_move[dat_len, n_subj];
}


parameters{
  real hyper_alpha; // learning rate hyperparameters
  real <lower=0> hyper_alpha_sd; // Learning rate standard deviation
  vector [n_subj] alpha_raw; // The individual learning rate

  real<lower=0> hyper_sigma_shape; // Hyperparameter for the reporting error shape
  real<lower=0> hyper_sigma_rate; // Hyperparameter for the reporting error rate
  vector<lower=0> [n_subj] sigma_raw;  // "Reporting error variance" parameter
}


transformed parameters{
  vector [n_subj] alpha;
  vector <lower=.02> [n_subj] sigma;
  // Non-centered parameterisation
  alpha = Phi(hyper_alpha + hyper_alpha_sd * alpha_raw);
  // Truncating sigma at .02 because otherwise subj 111 doesn't converge!
  // This seems to be slightly faster than truncating the parameter directly.
  sigma = sigma_raw + .02;
}


model{
  // Hyperpriors
  target += normal_lpdf(hyper_alpha | -.5, .5);
  target += gamma_lpdf(hyper_alpha_sd | 5, 10);

  target += gamma_lpdf(hyper_sigma_shape | 10, .3);
  target += gamma_lpdf(hyper_sigma_rate | 15, .2);

  for (i_subj in 1:n_subj){
    real model_belief;

    // individual priors
    target += std_normal_lpdf(alpha_raw[i_subj]);
    target +=  gamma_lpdf(sigma_raw[i_subj] | hyper_sigma_shape, hyper_sigma_rate);

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {
        model_belief = model_belief + alpha[i_subj] *
          (up_move[i_trial, i_subj] - model_belief);
      }
      target += normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma[i_subj]) -
        log_diff_exp(normal_lcdf(1 | model_belief, sigma[i_subj]),
                            normal_lcdf(0 | model_belief, sigma[i_subj]));
    }
  }
}
