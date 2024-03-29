// This file contains the model with hierarchical (and non-centered paramerized)
// alphas but a single sigma (reporting error) for all participants.

data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=1, upper=5> updating_from[dat_len, n_subj]; // Invest or short?
  int<lower=0, upper=1> up_move[dat_len, n_subj];
}


parameters{
  vector [5] hyper_alphas; // learning rate hyperparameters
  vector <lower=0> [5] hyper_alpha_sds; // Learning rate standard deviation
  matrix [n_subj, 5] alphas_raw; // The individual learning rate

  real<lower=0> sigma; // reporting error
}


transformed parameters{
  matrix [n_subj, 5] alphas;
  vector [5] hyper_alphas_transformed;
  // Non-centered parameterisation
  for (i in 1:5){
    alphas[:, i] = Phi(hyper_alphas[i] + hyper_alpha_sds[i] * alphas_raw[:, i]);
  }
  hyper_alphas_transformed = Phi(hyper_alphas);
}

model{

  // Priors following Fontanesi19
  // Hyperpriors
  for (i in 1:5){
    hyper_alphas[i] ~ normal(-.5, .5);
    hyper_alpha_sds[i] ~ gamma(1.2, 3);
  }

  sigma ~ gamma(1.2, 3);

  for (i_subj in 1:n_subj){
    real model_belief;
    
    // individual priors
    for (i in 1:5){
      alphas_raw[i_subj, :] ~ std_normal();
    }

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
        model_belief, sigma) -
        log_diff_exp(normal_lcdf(1 | model_belief, sigma),
                            normal_lcdf(0 | model_belief, sigma));
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
//         model_belief, sigma) -
//         log_diff_exp(normal_lcdf(1 | model_belief, sigma),
//                             normal_lcdf(0 | model_belief, sigma));
//     }
//   }
// }
