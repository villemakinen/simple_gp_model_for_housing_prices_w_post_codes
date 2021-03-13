
/*
  Stan program for simple GP model for the intercepts with a gaussian likelihood 
  21.12.2020
  Ville Mäkinen
*/
  
  functions {
    real generalized_inverse_gaussian_lpdf(real x, int p,
                                           real a, real b) {
      return p * 0.5 * log(a / b)
      - log(2 * modified_bessel_second_kind(p, sqrt(a * b)))
      + (p - 1) * log(x)
      - (a * x + b / x) * 0.5;
    }
  }
data {
  int<lower = 1> n_observations; 
  vector[n_observations] response; 
  
  int<lower = 1> n_explanatory_vars;  
  matrix[n_observations, n_explanatory_vars] design_matrix_X;  
  
  // group level information
  int<lower = 1> n_groups; 
  
  matrix[n_groups, n_groups] sq_distance_matrix;  
  
  int group_indicator[n_observations];
  
  // all post codes 
  /*int<lower = 1> n_all_post_codes; 
  
  matrix[n_all_post_codes, n_all_post_codes] sq_distance_matrix_all_post_codes;*/
}
parameters {
  vector[n_explanatory_vars] beta_coeffs;
  
  // variance parameters for the response
  real<lower = 0> sigma_resid;  // residual SDs
  
  // https://mc-stan.org/docs/2_25/stan-users-guide/fit-gp-section.html and https://mc-stan.org/docs/2_24/functions-reference/covariance.html
  // k(x_i, x_j) = \alpha^2 exp( -1/2*\rho^2 * \sum_d=1_D (x_i,d - x_j,d)^2) + \Kronecker_delta_ij sigma_gp
  // Banerjee et al: sigma_gp^2  = 'nugget', alpha^2 + sigma_gp^2 = 'sill' <=> alpha^2 = 'partial sill', rho = 'range' / 'decay'
  real<lower = 0> rho;  // length-scale param / should have a generalized inverse gaussian prior as discussed 10.3.1? 
  real<lower = 0> alpha; // 
  real<lower = 0> sigma_gp; 
  
  vector[n_groups] GP_offsets; 
}
model {
  vector[n_observations] EV;
  
  /****************************************************************************/
    // GP stuff for the groups
  matrix[n_groups, n_groups] covar_mat; 
  vector[n_groups] GP_group_effects;
  
  real alpha_sq = square(alpha); 
  real rho_sq = square(rho); 
  real sigma_gp_sq = square(sigma_gp);
  
  // GP intercept calculation - how to do these in parallel? 
    // off-diagonal element calculations
  for (i in 1:(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      // it is assumed that the inputs are given as squared distances!
        covar_mat[i, j] = alpha_sq*exp( -(1/(2*rho_sq)) * sq_distance_matrix[i, j] ); 
        covar_mat[j, i] = covar_mat[i, j];
    }
  }
  
  // diagonal elements calculation 
  for (i in 1:n_groups) {
    covar_mat[i, i] = sigma_gp_sq + alpha_sq; 
  }
  
  GP_group_effects = cholesky_decompose(covar_mat) * GP_offsets;
  
  /****************************************************************************/
    // prior definitions 
  
  // GP priors
  target += generalized_inverse_gaussian_lpdf(rho | 1, 1, 1);
  
  target += cauchy_lpdf(alpha | 0, 1)
  - 1 * cauchy_lccdf(0 | 0, 1);
  
  target += cauchy_lpdf(sigma_gp | 0, 1)
  - 1 * cauchy_lccdf(0 | 0, 1);
  
  // coefficient priors
  target += std_normal_lpdf(beta_coeffs);

  // variance parameter prior
  target += student_t_lpdf(sigma_resid | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  
  /****************************************************************************/
    // likelihood 
  
  target += std_normal_lpdf(GP_offsets);
  
  EV = design_matrix_X * beta_coeffs;
  
  for (k in 1:n_observations) {
    EV[k] += GP_group_effects[group_indicator[k]];
  }
  
  target += normal_lpdf(response | EV, sigma_resid);
}
generated quantities {
  vector[n_groups] GP_group_effects_gen_quan; 
  //vector[n_all_post_codes - n_groups] GP_group_effects_gen_quan_OOS; 
  vector[n_observations] log_lik; // log_lik-vector is for the loo-package 
  
  // local block to mask the covariance matrices etc. 
  // sampling crashes when cov. matrices are added in the gen. quant. block?!
    {
      vector[n_observations] EV;
      
      // covariance matrix parameters
      real alpha_sq = square(alpha); 
      real rho_sq = square(rho); 
      real sigma_gp_sq = square(sigma_gp);
      
      matrix[n_groups, n_groups] covar_mat_gen_quan; 
      
      
      /*
        int new_groups = n_all_post_codes - n_groups;
      
      // (partial) covariance matrices needed for the parameters for the predictive distribution of the OOS post codes
      matrix[n_groups, new_groups] obs_pred_mixed_covar_mat;
      matrix[new_groups, new_groups] pred_covar_mat;
      
      // parameters for the predictive distribution of the OOS post codes
      vector[new_groups] new_mu; 
      matrix[new_groups, new_groups] new_Sigma;
      
      */
        
        //****************************************************************************
        // GP stuff for the groups
      
      for (i in 1:(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          // it is assumed that the inputs are given as squared distances!
            covar_mat_gen_quan[i, j] = alpha_sq*exp( -(1/(2*rho_sq)) * sq_distance_matrix[i, j] ); 
            covar_mat_gen_quan[j, i] = covar_mat_gen_quan[i, j];
        }
      }
      
      // diagonal elements calculation 
      for (i in 1:n_groups) {
        covar_mat_gen_quan[i, i] = sigma_gp_sq + alpha_sq; 
      }
      
      GP_group_effects_gen_quan = cholesky_decompose(covar_mat_gen_quan) * GP_offsets;
      
      //****************************************************************************
        // log-likelihood calculations
      
      EV = design_matrix_X * beta_coeffs;
      
      for (k in 1:n_observations) {
        EV[k] += GP_group_effects_gen_quan[group_indicator[k]];
        
        log_lik[k] = normal_lpdf(response[k] | EV[k], sigma_resid);
      }
      
      //****************************************************************************
        // GP group effect prediction for all post codes 
      // see section 1.2 in https://betanalpha.github.io/assets/case_studies/gaussian_processes.html
      
      //matrix[n_groups, n_all_post_codes - n_groups] obs_pred_mixed_covar_mat;
      //matrix[n_all_post_codes - n_groups, n_all_post_codes - n_groups] pred_covar_mat;
      
      // calculation of the 'mixed' (partial) covariance matrix (K_mix)
      /*
        for (i in 1:n_groups) {
          for (j in 1:new_groups) {
            obs_pred_mixed_covar_mat[i, j] = alpha_sq*exp( -(1/(2*rho_sq)) * sq_distance_matrix_all_post_codes[i, n_groups + j] );
          }
        }
      
      // calculation of the covariance matrix for the OOS post codes (K_pred)
      
      // off-diagonal elements
      for (i in 1:(new_groups - 1)) {
        for (j in (i + 1):new_groups) {
          pred_covar_mat[i, j] = alpha_sq*exp( -(1/(2*rho_sq)) * sq_distance_matrix_all_post_codes[n_groups + i, n_groups + j] );
          pred_covar_mat[j, i] = pred_covar_mat[i, j];
        }
      }
      
      // diagonal elements 
      for (i in 1:new_groups) {
        pred_covar_mat[i, i] = sigma_gp_sq + alpha_sq;
      }
      
      // mean calculation for the predictive distribution for the group effects for the OOS post codes 
      // \mu = K_mix^T * K_obs^-1 * f_obs
      
      new_mu = obs_pred_mixed_covar_mat' * inverse_spd(covar_mat_gen_quan) * GP_group_effects_gen_quan; 
    // inverse instead of inverse_spd? 
    
    // covariance calculation for the predictive distribution for the group effects for the OOS post codes
    // \Sigma = K_pred - K_mix^T * K_obs^-1 * K_mix 
    
    new_Sigma = pred_covar_mat - obs_pred_mixed_covar_mat' * inverse(covar_mat_gen_quan) * obs_pred_mixed_covar_mat; 
      // inverse instead of inverse_spd? 
        
        // generate the predictive distributions
      
      GP_group_effects_gen_quan_OOS = multi_normal_rng(new_mu, new_Sigma);
      
      */
    }
}
