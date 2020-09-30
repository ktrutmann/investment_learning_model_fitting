functions{
    vector utility_fun(row_vector probabilities, row_vector money, real alpha){
        vector[3] utility;
        for(i in 1:num_elements(probabilities)){
            utility[i] = pow(money[i], alpha) * (probabilities[i]);
        }
        return utility;
    }
    vector softmax_fun(vector utilities, real beta){
        vector[num_elements(utilities)] outcome_probabilities;
        for(i in 1:num_elements(utilities)){
        outcome_probabilities[i] = exp(beta * utilities[i])/sum(exp(beta * utilities));
        }
        
        return outcome_probabilities;
        
    }
}
data{
    int<lower=1> TOTAL_TRIALS; // here put how many trials we are fitting
    int<lower=1> N_PP;
    int<lower=1> RESP[TOTAL_TRIALS];
    matrix[TOTAL_TRIALS, 3] PROBS;
    matrix[TOTAL_TRIALS, 3] MONEY;
    int<lower=1, upper = N_PP> PP_INDEX[TOTAL_TRIALS];
}
parameters{
    real alpha_mu;
    real<lower = 0> alpha_sigma;
    real beta_mu;
    real<lower = 0> beta_sigma;
    vector[N_PP] alpha_raw;
    vector[N_PP] beta_raw;
}  
transformed parameters {
    vector<lower = 0>[N_PP] alpha;
    vector<lower = 0>[N_PP] beta;
    real<lower = 0> alpha_t[TOTAL_TRIALS];
    real<lower = 0> beta_t[TOTAL_TRIALS];
    alpha = exp(alpha_mu + alpha_sigma * alpha_raw);
    beta = exp(beta_mu + beta_sigma * beta_raw);
    for(n in 1:TOTAL_TRIALS){
        alpha_t[n] = alpha[PP_INDEX[n]];
        beta_t[n] = beta[PP_INDEX[n]];
    }
}
model{
    vector[3] probs;
    alpha_mu ~ normal(0, 1);
    alpha_sigma ~ normal(0, 1);
    beta_mu ~ normal(0, 1);
    beta_sigma ~ normal(0, 1);
    alpha_raw ~ normal(0, 1);
    beta_raw ~ normal(0, 1);
    for(i in 1:TOTAL_TRIALS){
        probs = softmax_fun(utility_fun(PROBS[i,], MONEY[i,], alpha_t[i]), beta_t[i]);
        RESP[i] ~ categorical(probs); // that should be the generalization of the bernouli
    }
}
generated quantities{
    vector<lower = 1>[TOTAL_TRIALS] y_hat;
    vector[3] probs;
    for(i in 1:TOTAL_TRIALS){
        probs = softmax_fun(utility_fun(PROBS[i,], MONEY[i,], alpha_t[i]), beta_t[i]);
        y_hat[i] = categorical_rng(probs);
    }
}
