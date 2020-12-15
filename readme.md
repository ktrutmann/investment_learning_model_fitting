# Model recovery and fitting

## Recovery

The data in this step is generated by running the oTree bot of the experiment.


## Notes:
As stans `categorical` distribution takes on values ${1, ..., N}$ we add 2 to the portfolio to match this (i.e. shorting is now coded as 1 while holding is a 3).


## Fitting

### Fitting only on Beliefs
The `bayes_updater`and `multi_alpha_rl` stan files are used to fit only on the reported beliefs.
Especially the latter is needed for the MLA_Exp project where the parameters that are simulated are drawn from its posterior.
The learning rates are coded as follows:

- `alpha`: The base learning rate
- `rl_beta[1]`: The difference to the base learning rate when seeing an advantageous move while in a gain position
- `rl_beta[2]`: The difference to the base learning rate when seeing a disadvantageous move while in a gain position
- `rl_beta[3]`: The difference to the base learning rate when seeing a advantageous move while in a loss position
- `rl_beta[4]`: The difference to the base learning rate when seeing a disadvantageous move while in a loss position
- `sigma`: The variance of the reporting error before standardizing the belief.

The priors and hyperpriors were declared as in @Fontanesi19.

I thought about implementing the likelihood in the following way:
The model prediction as well as the data are transformed from the space $[0, 1]$ into $[-inf, inf]$ by the cumulative standard normal distribution. Here the likelihood is calculated using a normal distribution around the model prediction. The problem is, that a mismatch at the border, i.e. the model predicting a probability of .8 while the participant entered .9, is punished a lot stronger compared to the same mismatch near the center (i.e. model: .55; participant: .65). There is no reason why the model should have to be more precise toward the border.

Therefore the current implementation uses a truncated normal distribution in the range [0, 1].