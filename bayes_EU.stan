// This script fits a bayesian updater with expected utility preferences
// on the choice data of the experiment (ignoring the reported beliefs).

data{
  int<lower=1> dat_len;  // How many data points are there?
  int<lower=1> n_subj;  // How many participants are we fitting?
  int<lower=0, upper=75> round_in_block[dat_len, n_subj];
  int<lower=1, upper=3> hold[dat_len, n_subj]; // The Portfolio
  matrix<lower=0>[dat_len, n_subj] current_price; // The price in this round
  matrix<lower=0, upper=1>[dat_len, n_subj] bayes_probs; // The objective bayesian probability
}
// TODO: (1) Add EU evaluation and softmax
// TODO: (5) Make it hierarchical


parameters{
  real<lower=0> theta[n_subj];  // Sensitivity parameter for softmax
  real<lower=0, upper=2> beta_risk[n_subj];  // risk_aversion_parameter
}


model{
  // vector<lower=0>[3] prices_if_increase;
  // vector<lower=0>[3] prices_if_decrease;
  vector[3] portf_probs;

  for (i_subj in 1:n_subj) {

    // TODO: (1) Make it run each step of the way for debugging!
    // TODO: (1) Calculate the value of each possible portfolio
    // TODO: (9) See what's vectorizable here
    for (i_round in 1:dat_len){
      // prices_if_increase = [price[i_round, i_subj] + 5,
      //                       price[i_round, i_subj] + 10,
      //                       price[i_round, i_subj] + 15];
      // prices_if_descrease = [price[i_round, i_subj] - 5,
      //                       price[i_round, i_subj] - 10,
      //                       price[i_round, i_subj] - 15];
      portf_probs = [1 - bayes_probs[i_round, i_subj],
                    .5,
                    bayes_probs[i_round, i_subj]]';

      target += categorical_lpmf(hold[i_round, i_subj] |
        softmax(portf_probs));
      // TODO: (9) Check why categorical_lupmf does not work. Version issue?
      // TODO: (5) Use the "bugfixed" softmax
    }

  }
}


// generated quantities {
//     // TODO: (5) Update the generated quantities
//   matrix[dat_len, n_subj] log_lik;  // For loo evalutation later

//   for (i_subj in 1:n_subj){
//     for (i_trial in 1:dat_len) {
//       log_lik[i_trial, i_subj] = normal_lpdf(belief[i_trial, i_subj] |
//         bayes_probs[i_trial, i_subj], sigma[i_subj]);
//     }
//   }
// }
