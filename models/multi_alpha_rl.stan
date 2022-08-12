// This file contains the full model with hierarchical alphas and sigmas
// (reporting errors) which are parameterised as Non-centered.

data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=1, upper=5> updating_from[dat_len, n_subj]; // Current "context"
  int<lower=0, upper=1> up_move[dat_len, n_subj];
}


parameters{
  vector [5] hyper_alphas; // learning rate hyperparameters
  vector <lower=0> [5] hyper_alpha_sds; // Learning rate standard deviation
  matrix [n_subj, 5] alphas_raw; // The individual learning rate

  real<lower=0> hyper_sigma_shape; // Hyperparameter for the reporting error shape
  real<lower=0> hyper_sigma_rate; // Hyperparameter for the reporting error rate
  vector<lower=0> [n_subj] sigma_raw;  // "Reporting error variance" parameter
}


transformed parameters{
  matrix<lower=0, upper=1> [n_subj, 5] alphas;
  vector<lower=.02> [n_subj] sigma;
  // Non-centered parameterisation
  for (i in 1:5){
    alphas[:, i] = Phi(hyper_alphas[i] + hyper_alpha_sds[i] * alphas_raw[:, i]);
  }
  // Truncating sigma at .02 because otherwise subj 111 doesn't converge!
  // This seems to be slightly faster than truncating the parameter directly.
  sigma = sigma_raw + .02;
}

model{
  // Priors following Fontanesi19
  // Hyperpriors
  for (i in 1:5){
    hyper_alphas[i] ~ normal(-.5, .5);
    hyper_alpha_sds[i] ~ gamma(5, 10);
  }

  hyper_sigma_shape ~ gamma(10, .3);
  hyper_sigma_rate ~ gamma(15, .2);
  
  for (i_subj in 1:n_subj){
    real model_belief;
    
    // individual priors
    for (i in 1:5){
      alphas_raw[i_subj, :] ~ std_normal();
    }
    sigma_raw[i_subj] ~ gamma(hyper_sigma_shape, hyper_sigma_rate);

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {
        model_belief = model_belief +
          alphas[i_subj, updating_from[i_trial, i_subj]] *
          (up_move[i_trial, i_subj] - model_belief);
      }
      // Normal truncated by [0, 1]
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
//         model_belief = model_belief +
//           alphas[i_subj, updating_from[i_trial, i_subj]] *
//           (up_move[i_trial, i_subj] - model_belief);
//       }

//     log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
//         model_belief, sigma[i_subj]) -
//         log_diff_exp(normal_lcdf(1 | model_belief, sigma[i_subj]),
//                             normal_lcdf(0 | model_belief, sigma[i_subj]));
//     }
//   }
// }
