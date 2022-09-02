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
  real<lower=0> scaling_param;  // Individual Scaling factor
  real<lower=0> sigma;  // "Reporting error variance" parameter
}


model{
  // Priors
  target += gamma_lpdf(scaling_param | 10, 10);
  target += gamma_lpdf(sigma | 33, 75);

  for (i_subj in 1:n_subj) {
    target += normal_lpdf(belief[:, i_subj] |
      (bayes_probs[:, i_subj] - .5) * scaling_param + .5, sigma);
  }
}
