// This file contains the full model with hierarchical alphas and sigmas
// (reporting errors) which are parameterised as Non-centered.

data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=0, upper=1> up_move[dat_len, n_subj];
}


parameters{
  real alpha_raw; // learning rate parameters
  real<lower=0> sigma; // reporting error
}


transformed parameters{
  real alpha;
  alpha = Phi(alpha_raw);
}

model{

  // Priors following Fontanesi19
  alpha_raw ~ normal(-.5, .5);
  sigma ~ gamma(1.2, 3);

  for (i_subj in 1:n_subj){
    real model_belief;
    
    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {
        model_belief = model_belief +
          alpha * (up_move[i_trial, i_subj] - model_belief);
      }
      // Normal truncated by [0, 1]
      target += normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma) -
        log_diff_exp(normal_lcdf(1 | model_belief, sigma),
                            normal_lcdf(0 | model_belief, sigma));
    }
  }
}