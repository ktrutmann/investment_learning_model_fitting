functions{
  real update_model_belief(real prev_belief, int invested, int gain_pos,
                      int loss_pos, int favorable_move, row_vector alphas) {

    if (gain_pos && favorable_move) {
      return prev_belief + alphas[2] * (favorable_move - prev_belief);

    } else if (gain_pos && !favorable_move) {
      return prev_belief + alphas[3] * (favorable_move - prev_belief);

    } else if (loss_pos && favorable_move) {
      return prev_belief + alphas[4] * (favorable_move - prev_belief);

    } else if (loss_pos && !favorable_move) {
      return prev_belief + alphas[5] * (favorable_move - prev_belief);

    } else {
      return prev_belief + alphas[1] * (favorable_move - prev_belief);
    }
  }
}


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
  vector [5] hyper_alphas; // learning rate hyperparameters
  vector <lower=0> [5] hyper_alpha_sds; // Learning rate standard deviation
  matrix [n_subj, 5] alphas_raw; // The individual learning rate

  real<lower=0> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> hyper_sigma_sd; // Hyperparameter for the reporting error
  vector<lower=0> [n_subj] sigmas_raw;  // "Reporting error variance" parameter
}


transformed parameters{
  // TODO: (3) Maybe redefine as array of vectors for efficiency?
  matrix [n_subj, 5] alphas;
  vector [n_subj] sigma;
  vector [5] transf_hyper_alpha;
  // Non-centered parameterisation
  for (i in 1:5){
    alphas[:, i] = Phi(hyper_alphas[i] + hyper_alpha_sds[i] * alphas_raw[:, i]);
  }
  sigma = hyper_sigma + hyper_sigma_sd * sigmas_raw;

  transf_hyper_alpha = Phi(hyper_alphas); // For easier interpretation of the output
}

model{
  // Priors following Fontanesi19
  // TODO: (2) Change parameters to be sensible in inv_Phi space

  // Hyperpriors
  for (i in 1:5){
    hyper_alphas[i] ~ normal(.3, .3);
    hyper_alpha_sds[i] ~ gamma(1.2, 3);
  }

  hyper_sigma ~ gamma(1.2, 3);
  hyper_sigma_sd ~ gamma(1.2, 3);

  for (i_subj in 1:n_subj){
    real model_belief;

    // individual priors
    for (i in 1:5){
      alphas_raw[i_subj, :] ~ std_normal();
    }
    sigmas_raw ~ std_normal();

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {

        model_belief = update_model_belief(
          model_belief,
          invested[i_trial, i_subj],
          gain_position[i_trial, i_subj],
          loss_position[i_trial, i_subj],
          favorable_move[i_trial, i_subj],
          alphas[i_subj, :]);
      }
    // TODO: (5) Put the model loop into the transformed params. section?
    belief[i_trial, i_subj] ~ normal(model_belief, sigma[i_subj]) T[0, 1];
    }
  }
}


// generated quantities {
//   // To "test" the differences:
//   matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

//   for (i_subj in 1:n_subj){
//     real model_belief;  // The belief the model would hold

//     for (i_trial in 1:dat_len) {
//       if (round_in_block[i_trial, i_subj] == 0) {
//         model_belief = .5;
//       } else {
//         model_belief = update_model_belief(
//           model_belief,
//           invested[i_trial, i_subj],
//           gain_position[i_trial, i_subj],
//           loss_position[i_trial, i_subj],
//           favorable_move[i_trial, i_subj],
//           alpha[i_subj],
//           rl_beta[i_subj, :]);
//       }

//     log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
//       model_belief, sigma[i_subj]);
//     // TODO: (3) Does this have to be truncated? / Do I need it for beliefs?
//     }
//   }
// }
