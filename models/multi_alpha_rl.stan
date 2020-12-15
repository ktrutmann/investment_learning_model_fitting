functions{
  real update_model_belief(real prev_belief, int invested, int gain_pos,
                      int loss_pos, int favorable_move, real alpha,
                      real[] betas) {

      if (gain_pos && favorable_move) {
        return prev_belief + (alpha + betas[1]) *
          (favorable_move - prev_belief);

      } else if (gain_pos && !favorable_move) {
        return prev_belief + (alpha + betas[2]) *
          (favorable_move - prev_belief);

      } else if (loss_pos && favorable_move) {
        return prev_belief + (alpha + betas[3]) *
          (favorable_move - prev_belief);

      } else if (loss_pos && !favorable_move) {
        return prev_belief + (alpha + betas[4]) *
          (favorable_move - prev_belief);

      } else {
        return prev_belief + alpha * (favorable_move - prev_belief);
      }
  }
}


data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=-1, upper=1> invested[dat_len, n_subj]; // Invest or short?
  int<lower=0, upper=1> gain_position[dat_len, n_subj];
  int<lower=0, upper=1> loss_position[dat_len, n_subj];
  int<lower=0, upper=1> favorable_move[dat_len, n_subj];
}


parameters{
  real hyper_alpha; // Hyperparameter for the learning rate
  real<lower=0> hyper_alpha_sd; // Learning rate standard deviation
  real<lower=-2, upper=2> hyper_beta[4];  // The "diminishing" learning param.
  real<lower=0> hyper_beta_sd[4];
  real<lower=0, upper=1> alpha[n_subj];  // The individual learning rate
  real<lower=-2, upper=2> rl_beta[n_subj, 4];  // The individual "diminishing" learning param.

  real<lower=0, upper=1> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0, upper=1> hyper_sigma_sd; // Hyperparameter for the reporting error
  real<lower=0> sigma[n_subj];  // "Reporting error variance" parameter
}


// transformed parameters{

// }


model{
  // Priors following Fontanesi19
  // TODO: (5) Change priors to be centered around the results of Gershman16? 
  // TODO: (1) Make sure the restrictions of the priors matches that of the parameters!

  // Hyperpriors
  hyper_alpha ~ normal(0, .8);
  hyper_alpha_sd ~ normal(0, .5);  // Half normal
  for (i in 1:4){
    hyper_beta[i] ~ normal(0, .5);
    hyper_beta_sd[i] ~ normal(0, .5);
  }
  hyper_sigma ~ gamma(1.2, 1.2);
  hyper_sigma_sd ~ gamma(1.2, 1.2);

  for (i_subj in 1:n_subj){
    real model_belief;

    // individual priors
    alpha[i_subj] ~ normal(hyper_alpha, hyper_alpha_sd);
    rl_beta[i_subj, :] ~ normal(hyper_beta[:], hyper_beta_sd[:]);
    sigma[i_subj] ~ normal(hyper_sigma, hyper_sigma_sd);


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
          alpha[i_subj],
          rl_beta[i_subj, :]);
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
//     // TODO: (3) How can this be truncated?
//     }
//   }
// }
