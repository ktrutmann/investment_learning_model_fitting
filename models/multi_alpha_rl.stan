functions{
  real update_model_belief(real prev_belief, int invested, int gain_pos,
                      int loss_pos, int price_increase, real[] alphas) {

      if (gain_pos && price_increase || loss_pos && !price_increase) {

        return prev_belief +
          alphas[2] * (price_increase -
          prev_belief);

      } else {

        return prev_belief +
          alphas[1] * (price_increase -
          prev_belief);
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
  int<lower=0, upper=1> price_increase[dat_len, n_subj];
}
 

parameters{
  real<lower=0, upper=1> hyper_alpha[2]; // Hyperparameter for the learning rate
  // TODO: Code these as hyper_alpha_basic and hyper_alpha_additional which is relative to the basic one (non-centered hyper parameters)
  real<lower=0, upper=1> alpha[n_subj, 2];  // The learning rate
  real<lower=0, upper=1> hyper_sigma; // Hyperparameter for the reporting error
  real<lower=0> sigma[n_subj];  // "Reporting error variance" parameter
}


model{
  
  // Priors
  hyper_alpha ~ normal(.2, .5);  // Truncated normal around "rational" value
  hyper_sigma ~ gamma(1.2, 1.2);

  for (i_subj in 1:n_subj){
    real model_belief;  // The belief the model would hold

    alpha[i_subj, :] ~ normal(hyper_alpha, .5);
    sigma[i_subj] ~ normal(hyper_sigma, .5);  // "Reporting error variance"


    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {

        model_belief = .5;

      } else {

        model_belief = update_model_belief(
          model_belief,
          invested[i_trial, i_subj],
          gain_position[i_trial, i_subj],
          loss_position[i_trial, i_subj],
          price_increase[i_trial, i_subj],
          alpha[i_subj]);
      }
    // TODO: Put the model loop into the transformed params. section

    // TODO: Use non-centered parameterization here? But how?
    target += normal_lpdf(belief[i_trial, i_subj] |
      model_belief, sigma[i_subj]);
    }
  }

}


generated quantities {
  // To "test" the differences:
  real hyper_alpha_diff;
  real alpha_diff[n_subj];
  matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

  hyper_alpha_diff = hyper_alpha[1] - hyper_alpha[2];

  for (i_subj in 1:n_subj){
    real model_belief;  // The belief the model would hold

    alpha_diff[i_subj] = alpha[i_subj, 1] - alpha[i_subj, 2];

    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {

        model_belief = .5;

      } else {

        model_belief = update_model_belief(
          model_belief,
          invested[i_trial, i_subj],
          gain_position[i_trial, i_subj],
          loss_position[i_trial, i_subj],
          price_increase[i_trial, i_subj],
          alpha[i_subj]);
      }

      log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma[i_subj]);
    }
  }
}
