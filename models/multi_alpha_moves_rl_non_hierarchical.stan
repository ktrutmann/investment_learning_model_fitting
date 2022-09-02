data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  matrix<lower=0, upper=1>[dat_len, n_subj] belief; // The reported beliefs
  int<lower=1, upper=5> updating_from[dat_len, n_subj]; // Coded context
  int<lower=0, upper=1> up_move[dat_len, n_subj];
}


parameters{
  vector [3] alphas_raw; // learning rate hyperparameters
  real<lower=0> sigma;  // "Reporting error variance" parameter
}


transformed parameters{
  vector [3] alphas;
  alphas = Phi(alphas_raw);
}


model{
  // Priors following Fontanesi19
  // Hyperpriors
  for (i in 1:3){
    target += normal_lpdf(alphas_raw[i] | -.5, .3);
  }
  target += gamma_lpdf(sigma | 33, 75);
  
  for (i_subj in 1:n_subj){
    real model_belief;
    
    for (i_trial in 1:dat_len) {
      if (round_in_block[i_trial, i_subj] == 0) {
        model_belief = .5;
      } else {
        // Updating model beliefs:
        if (updating_from[i_trial, i_subj] == 2 ||
            updating_from[i_trial, i_subj] == 4) {
          model_belief = model_belief + alphas[2] *
            (up_move[i_trial, i_subj] - model_belief);
        } if (updating_from[i_trial, i_subj] == 3 ||
            updating_from[i_trial, i_subj] == 5) {
          model_belief = model_belief + alphas[3] *
            (up_move[i_trial, i_subj] - model_belief);
        } else {
          model_belief = model_belief + alphas[1] *
            (up_move[i_trial, i_subj] - model_belief);
        }
      }

      // Normal truncated by [0, 1]
      target += normal_lpdf(belief[i_trial, i_subj] |
        model_belief, sigma) -
        log_diff_exp(normal_lcdf(1 | model_belief, sigma),
                            normal_lcdf(0 | model_belief, sigma));
    }
  }
}
