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

  real<lower=0> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> hyper_sigma_sd; // Hyperparameter for the reporting error
  // real<lower=0> hyper_sigma_prior; // Hyperparameter for the reporting error
  // real<lower=0> hyper_sigma_sd_prior; // Hyperparameter for the reporting error
  vector <lower=0> [n_subj] sigmas_raw;  // "Reporting error variance" parameter
  // real<lower=0> sigma_prior_raw;
}


transformed parameters{
  vector [n_subj] alpha;
  vector <lower=0> [n_subj] sigma;
  // real <lower=0> sigma_prior;
  // Non-centered parameterisation
  alpha = Phi(hyper_alpha + hyper_alpha_sd * alpha_raw);
  sigma = hyper_sigma + hyper_sigma_sd * sigmas_raw;
  for (i in 1:n_subj) {
    sigma[i] = max([.02, sigma[i]]');
  }
  // sigma_prior = hyper_sigma_prior + hyper_sigma_sd_prior * sigma_prior_raw;
}

model{
  // Hyperpriors
  hyper_alpha ~ normal(-.5, .5);
  hyper_alpha_sd ~ gamma(5, 10);

  hyper_sigma ~ gamma(5, 10);
  hyper_sigma_sd ~ gamma(5, 10);
  // hyper_sigma_prior ~ gamma(5, 10);
  // hyper_sigma_sd_prior ~ gamma(5, 10);

  for (i_subj in 1:n_subj){
    real model_belief;

    // individual priors
    alpha_raw[i_subj] ~ std_normal();
    sigmas_raw[i_subj] ~ std_normal();
    // sigma_prior_raw ~ std_normal();

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

// generated quantities {
//   matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

//   for (i_subj in 1:n_subj){
//     real model_belief;

//     for (i_trial in 1:dat_len) {
//       if (round_in_block[i_trial, i_subj] == 0) {
//         model_belief = .5;
//       } else {
//         model_belief = model_belief + alpha[i_subj] *
//           (up_move[i_trial, i_subj] - model_belief);
//       }

//     log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
//         model_belief, sigma[i_subj]) -
//         log_diff_exp(normal_lcdf(1 | model_belief, sigma[i_subj]),
//                             normal_lcdf(0 | model_belief, sigma[i_subj]));
//     }
//   }
// }
