/*
  Stan program for simple GP model for the intercepts with a gaussian likelihood 
  31.1.2021
  Ville Mäkinen
*/

data {
  int<lower = 1> n_observations; 
  vector[n_observations] response; 
  
  int<lower = 1> n_explanatory_vars;  
  matrix[n_observations, n_explanatory_vars] design_matrix_X;  
  
  // group level information
  int<lower = 1> n_groups; 
  int group_indicator[n_observations];
}
parameters {
  vector[n_explanatory_vars] beta_coeffs;
  real intercept;
  
  // variance parameters for the response
  real<lower = 0> sigma_resid;  // residual SDs
  
  // parameters for the group effects
  //real mu_pop; 
  real<lower = 0> sigma_pop; 
  
  vector[n_groups] group_effect_offsets;
}
model {
  vector[n_observations] EV; 
  vector[n_groups] group_effects;
  
  // priors  
  target += normal_lpdf(intercept | 11, 5);
  
  target += std_normal_lpdf(beta_coeffs);

  //target += std_normal_lpdf(mu_pop);
  target += cauchy_lpdf(sigma_pop | 0, 1)
    - 1 * cauchy_lccdf(0 | 0, 1);
  
  target += cauchy_lpdf(sigma_resid | 0, 1)
    - 1 * cauchy_lccdf(0 | 0, 1);
  
  // group effect calculations 
  target += std_normal_lpdf(group_effect_offsets);
  
  //group_effects = mu_pop + sigma_pop*group_effect_offsets;
  group_effects = sigma_pop*group_effect_offsets;
  
  
  // likelihood 
  EV = intercept + design_matrix_X * beta_coeffs;
  
  for (k in 1:n_observations) {
    EV[k] += group_effects[group_indicator[k]]; 
  }

  target += normal_lpdf(response | EV, sigma_resid);
}
generated quantities {
  vector[n_groups] group_effects_gen_quan;
  
  vector[n_observations] log_lik; // log_lik-vector is for the loo-package 

  //group_effects_gen_quan = mu_pop + sigma_pop*group_effect_offsets;
  group_effects_gen_quan = sigma_pop*group_effect_offsets;
  {
    vector[n_observations] EV;
    
    //****************************************************************************
    // log-likelihood calculations
    
    EV = intercept + design_matrix_X * beta_coeffs;
  
    for (k in 1:n_observations) {
      EV[k] += group_effects_gen_quan[group_indicator[k]];
      
      log_lik[k] = normal_lpdf(response[k] | EV[k], sigma_resid);
    }
  }
}
