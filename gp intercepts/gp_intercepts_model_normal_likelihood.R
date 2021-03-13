
# Model with GP intercepts, normal likelihood  
# 19.12.2020
# Ville Mäkinen

setwd("/home/asdf/Desktop/asuntojen hinnat/")
rm(list = ls())
gc()

training_data <- read.csv2("./git/simple_gp_model_for_housing_prices_w_post_codes/data/training_data.csv")
map_data <- read.csv("./git/simple_gp_model_for_housing_prices_w_post_codes/post codes map/post_code_centroids.csv")

###############################################################################
# Data preparation
training_data$log_price <- log(training_data$price)

training_data$post_code <- as.character(training_data$post_code)
map_data$posti_alue <- as.character(map_data$posti_alue)

pad_postcode <- function(x) {
  padding_length <- 5 - nchar(x)
  
  padding <- paste(rep("0", padding_length), collapse="")
  return(paste(padding, x, collapse = "", sep = ""))
}

training_data$padded_post_code <- sapply(training_data$post_code, pad_postcode)
map_data$padded_post_code <- sapply(map_data$posti_alue, pad_postcode)

# chosen explanatory variables are centered and scaled with 2 sigma 

explanatory_var_names <- c("square_meters",
                           "own_property_dummy",
                           "row_house_dummy",
                           "town_house_dummy",
                           "sauna_dummy",
                           "condition_unrecorded_dummy",
                           "condition_good_dummy",
                           "condition_adequate_dummy",
                           "age_of_building")

calculate_scaling_parameters <- function(var_name, data) {
  var_mean <- mean(data[[var_name]])
  var_sd <- sd(data[[var_name]])
  
  return(c(var_mean, var_sd))
}

scaling_parameters <- lapply(X = explanatory_var_names, 
                             FUN = calculate_scaling_parameters, 
                             data = training_data)
names(scaling_parameters) <- explanatory_var_names

center_and_scale_variables <- function(var_name, data, input_scaling_parameters) {
  var_mean <- input_scaling_parameters[[var_name]][1]
  var_sd <- input_scaling_parameters[[var_name]][2]
  
  result_var <- (data[[var_name]] - var_mean)/(2*var_sd)
  
  return(result_var)
}

scaled_var_list <- lapply(X = explanatory_var_names, 
                          FUN = center_and_scale_variables,
                          data = training_data, 
                          input_scaling_parameters = scaling_parameters)

scaled_data <- data.frame(do.call(cbind, scaled_var_list))
names(scaled_data) <- paste(explanatory_var_names, "_stdized", sep = "")

training_data <- cbind(training_data, scaled_data)

# whole list of post codes is fetched to set the levels for the post code factor

post_code_muni_mapping_df <- read.csv2("./git/simple_gp_model_for_housing_prices_w_post_codes/paavo aineisto/post_code_muni_name_mapping.csv", 
                                       stringsAsFactors = F)

library(dplyr)
all_post_codes <- post_code_muni_mapping_df$post_code %>% sapply(pad_postcode) %>% unique

training_data$padded_post_code <- factor(training_data$padded_post_code, levels = all_post_codes)

# library(hash)
# 
# xcoord_hash <- hash(keys = map_data$padded_post_code, values = map_data$xcoord)
# ycoord_hash <- hash(keys = map_data$padded_post_code, values = map_data$ycoord)
# 
# training_data$xcoord <- sapply(as.character(training_data$padded_post_code),
#                                function(x) xcoord_hash[[x]])
# 
# training_data$ycoord <- sapply(as.character(training_data$padded_post_code),
#                                function(x) ycoord_hash[[x]])

###############################################################################
# Distance matrix for the post codes - used for fake data generation

distance_matrix <- read.csv("./post codes map/distance_matrix_GIS.csv")
# calculated euclidean distances using GIS...

row_post_code <- as.character(distance_matrix[, 1])
col_post_code <- as.character(distance_matrix[1, ])

row_post_code <- row_post_code[-1]
col_post_code <- col_post_code[-1]

row_post_code <- sapply(row_post_code, pad_postcode)
col_post_code <- sapply(col_post_code, pad_postcode)

distance_matrix <- distance_matrix[2:nrow(distance_matrix), 2:ncol(distance_matrix)]
rownames(distance_matrix) <- row_post_code
colnames(distance_matrix) <- col_post_code

sq_distance_matrix <- (distance_matrix/1000)^2

sq_distance_matrix_all <- sq_distance_matrix

found_padded_post_code_vec <- sort(as.character(unique(training_data$padded_post_code)))

# limit only to those post codes with observations in the training data for estimation 
sq_distance_matrix <- sq_distance_matrix[rownames(sq_distance_matrix) %in% found_padded_post_code_vec, ]
sq_distance_matrix <- sq_distance_matrix[, colnames(sq_distance_matrix) %in% found_padded_post_code_vec]

# reform the distance matrix to conform to (2.19) in Rasmussen, Williams 
all_post_codes_vec <- rownames(distance_matrix)

OOS_post_codes_vec <- sort(all_post_codes_vec[!(all_post_codes_vec %in% found_padded_post_code_vec)])

reformed_distance_matrix <- (distance_matrix/1000)^2

reformed_distance_matrix <- reformed_distance_matrix[, c(found_padded_post_code_vec, OOS_post_codes_vec)]
reformed_distance_matrix <- reformed_distance_matrix[c(found_padded_post_code_vec, OOS_post_codes_vec), ]
reformed_distance_matrix <- as.matrix(reformed_distance_matrix)

###############################################################################
# Prior checking with fake data generation 
#   The idea is to run the code in this section multiple times and visually 
#   check the price histogram 

library(extraDistr)

intercept_draw <- rnorm(1, mean = 11, sd = 5)

square_meters_coef_draw <- rnorm(1, mean = 0,sd = 1)
own_property_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1) 
row_house_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1)
town_house_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1)
sauna_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1)
condition_unrecorded_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1)
condition_good_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1)
condition_adequate_dummy_coef_draw <- rnorm(1, mean = 0,sd = 1)
age_of_building_coef_draw <- rnorm(1, mean = 0,sd = 1)

# vaihto Stanin dokumentaation parametrinimiin 
#     phi_sq:lla tulisi olla generalized inverse Gaussian-jakauma/Betancourt?

library(GeneralizedHyperbolic)
# x_seq <- seq(0, 10, length.out = 1000)
# plot(x_seq, dgig(x_seq), type = 'l')
# plot(x_seq, dgig(x_seq, psi = 2), type = 'l')
# plot(x_seq, dgig(x_seq, chi = 2), type = 'l')
# plot(x_seq, dgig(x_seq, lambda = 2), type = 'l')
# plot(x_seq, dgig(x_seq), type = 'l') 
# plot(x_seq, dhcauchy(x_seq), type = 'l') # half-cauchy assigns much mass around 0 => not desirable? 

# half-cauchy or truncated student's t for the other parameters? 
# plot(x_seq, dhcauchy(x_seq), type = 'l', ylim = c(0, 3)); abline(h = 0, lty = 2)
# plot(x_seq, 6.3*dt(x_seq, df = 3), type = 'l', ylim = c(0, 3)); abline(h = 0, lty = 2)
  # half-cauchy has fatter tail ? 
  

# GIG-parameters to be chosen on prior predictive checks, if possible? 
rho <- rgig(n = 1)  
alpha <- rhcauchy(n = 1, 1)

sigma_gp <- rhcauchy(n = 1, 1)

# k(x_i, x_j) = \alpha^2 exp( -1/2*\rho^2 * \sum_d=1_D (x_i,d - x_j,d)^2) + \Kronecker_delta_ij sigma_gp^2

# library(doParallel)
# library(foreach)
# 
# registerDoParallel(4)
# covariance_matrix <- foreach (k = 1:nrow(reformed_distance_matrix), .combine = rbind) %dopar% {
#   row_results <- rep(NA, ncol(reformed_distance_matrix))
#   for (l in 1:ncol(reformed_distance_matrix)) {
#     row_results[l] <- (alpha^2)*exp(-(1/(2*rho^2))*reformed_distance_matrix[k, l])
#     
#     if (k == l) {
#       row_results[l] <- row_results[l] + sigma_gp^2
#     }
#   }
#  
#   row_results  
# }
# stopImplicitCluster()
# 
# test_function <- function() {
#   k <- sample(1:nrow(covariance_matrix), 1)
#   l <- sample(1:ncol(covariance_matrix), 1)
#   diff <- covariance_matrix[k, l] -  ((alpha^2)*exp(-(1/(2*rho^2))*reformed_distance_matrix[k, l]) + ifelse(k == l, sigma_gp^2, 0))
#   
#   cat("k:", k, "l:", l, "diff:", diff, "\n")
# }
# test_function()

covariance_matrix <- (alpha^2)*exp(-(1/(2*rho^2))*reformed_distance_matrix)
covariance_matrix <- covariance_matrix + diag(rep(sigma_gp^2, nrow(covariance_matrix)))

library(MASS)
group_effects <- mvrnorm(n = 1, mu = rep(0, nrow(covariance_matrix)), Sigma = covariance_matrix)

# EV for the fake data
beta_vec <- matrix(c(square_meters_coef_draw,
                     own_property_dummy_coef_draw,
                     row_house_dummy_coef_draw,
                     town_house_dummy_coef_draw,
                     sauna_dummy_coef_draw,
                     condition_unrecorded_dummy_coef_draw,
                     condition_good_dummy_coef_draw,
                     condition_adequate_dummy_coef_draw,
                     age_of_building_coef_draw), ncol = 1)

EV_fake_data <- intercept_draw + group_effects[as.character(training_data$padded_post_code)] + as.matrix(scaled_data) %*% beta_vec 

# response generation for the fake data 
sigma_draw <- rhcauchy(n = 1, 1)

fake_log_price_draw <- EV_fake_data + sigma_draw*rnorm(n = length(EV_fake_data))

fake_price_draw <- exp(fake_log_price_draw)

# visual check whether chosen priors produce 'reasonable' prices
graphing_breaks <- quantile(fake_price_draw, probs = c(0.05, 0.95))

hist(fake_price_draw[fake_price_draw > graphing_breaks[1] & 
                       fake_price_draw < graphing_breaks[2]], nclass = 100)

library(hash)
indicator_hash <- hash(keys = rownames(sq_distance_matrix), 
                       values = 1:nrow(sq_distance_matrix))

indicator_rev_hash <- hash(keys = as.character(1:nrow(sq_distance_matrix)), 
                           values = rownames(sq_distance_matrix))

group_indicator <- sapply(as.character(training_data$padded_post_code), 
                          function(x) indicator_hash[[x]])

###############################################################################
# Fake data fit 

library(rstan)

stan_program <- stan_model(file = "./gp intercepts/gp_intercepts_model_normal_likelihood_no_intercept.stan")

fake_data_stan_fit <- sampling(object = stan_program, 
                               data = list(n_observations = nrow(training_data), 
                                           response = as.numeric(fake_log_price_draw) - mean(as.numeric(fake_log_price_draw)), 
                                           n_explanatory_vars = ncol(scaled_data), 
                                           design_matrix_X = as.matrix(scaled_data),
                                           
                                           # GP parameters (restricted in 'in-sample' post codes)
                                           n_groups = nrow(sq_distance_matrix), 
                                           sq_distance_matrix = as.matrix(sq_distance_matrix), 
                                           group_indicator = group_indicator
                                           # ,
                                           # 
                                           # # distances for extrapolating intercepts for OOS post codes
                                           # n_all_post_codes = nrow(reformed_distance_matrix), 
                                           # sq_distance_matrix_all_post_codes = reformed_distance_matrix
                                           ),
                               cores = 4, 
                               iter = 1000)
save(fake_data_stan_fit, file = "fake_data_fit.RData")
# print(fake_data_stan_fit)

pairs(fake_data_stan_fit, pars = c("rho", "alpha", "sigma_gp"
                                   # , "intercept"
                                   ))

print(fake_data_stan_fit, pars = "beta_coeffs"); beta_vec
# print(fake_data_stan_fit, pars = "intercept"); intercept_draw
print(fake_data_stan_fit, pars = c("rho", "alpha", "sigma_gp")); rho; alpha; sigma_gp
print(fake_data_stan_fit, pars = "sigma_resid"); sigma_draw

posterior_sample_fake_data <- as.matrix(fake_data_stan_fit)
group_effects_posterior_sample <- posterior_sample_fake_data[, grep("gen_quan", colnames(posterior_sample_fake_data))]

# checks for the group effects

post_code_names <- sapply(1:nrow(sq_distance_matrix), function(k) indicator_rev_hash[[as.character(k)]])
plot(group_effects[post_code_names], colMeans(group_effects_posterior_sample))
abline(a = 0, b = 1, col = 'red', lty = 2, lwd = 2)

problematic_post_codes <- head(sort(abs(group_effects[post_code_names] - colMeans(group_effects_posterior_sample)), decreasing = T), 50)
k <- 1
target_post_code <- names(problematic_post_codes)[k]
group_effects[target_post_code]

hist(group_effects_posterior_sample[, indicator_hash[[target_post_code]]], 
     xlim =  range(c(group_effects[target_post_code], group_effects_posterior_sample[, indicator_hash[[target_post_code]]])))
abline(v = group_effects[target_post_code], lwd = 2, lty = 2, col = 'red')
abline(v = mean(group_effects_posterior_sample[, indicator_hash[[target_post_code]]]), lwd = 2, lty = 2, col = 'darkgreen')
(table(training_data$padded_post_code))[target_post_code]

# checks for the OOS group effects in the fake data 
generate_OOS_gp_effects <- function(k, posterior_sample) {
  alpha_draw <- posterior_sample[k, "alpha"]
  rho_draw <- posterior_sample[k, "rho"]
  sigma_gp_draw <- posterior_sample[k, "sigma_gp"]
  observed_f_draw <- posterior_sample[k, grep("GP_group_effects_gen_quan", colnames(posterior_sample))]
  
  K_obs_cov_mat <- (alpha_draw^2)*exp(-(1/(2*rho_draw^2))*reformed_distance_matrix[found_padded_post_code_vec, found_padded_post_code_vec])
  K_mix_cov_mat <- (alpha_draw^2)*exp(-(1/(2*rho_draw^2))*reformed_distance_matrix[found_padded_post_code_vec, OOS_post_codes_vec])
  K_pred_cov_mat <- (alpha_draw^2)*exp(-(1/(2*rho_draw^2))*reformed_distance_matrix[OOS_post_codes_vec, OOS_post_codes_vec]) 
  
  K_obs_cov_mat <- K_obs_cov_mat + diag(rep(sigma_gp_draw^2, nrow(K_obs_cov_mat)))
  K_pred_cov_mat <- K_pred_cov_mat  + diag(rep(sigma_gp_draw^2, nrow(K_pred_cov_mat)))
  
  K_obs_cov_mat_inverse <- solve(K_obs_cov_mat)
  
  # calculate \mu-term 
  # // \mu = K_mix^T * K_obs^-1 * f_obs
  mu_vec_draw <- t(K_mix_cov_mat) %*% K_obs_cov_mat_inverse %*% observed_f_draw
  
  # calculate \Sigma-term
  #// \Sigma = K_pred - K_mix^T * K_obs^-1 * K_mix 
  Sigma_mat_draw <- K_pred_cov_mat - t(K_mix_cov_mat) %*% K_obs_cov_mat_inverse %*% K_mix_cov_mat
  
  # draw a sample 
  OOS_post_codes_group_effects_draw <- mvrnorm(n = 1, mu = mu_vec_draw, Sigma = Sigma_mat_draw)
  
  OOS_post_codes_group_effects_draw
}


library(doParallel)
library(foreach)

# registerDoParallel(10)
# OOS_gp_effects_pred_sample <- foreach (k = 1:nrow(posterior_sample_fake_data), .combine = rbind) %dopar% {
#   generate_OOS_gp_effects(k, posterior_sample = posterior_sample_fake_data)
# }
# stopImplicitCluster()
# 
# mean_pred_gp_effects <- colMeans(OOS_gp_effects_pred_sample)
# plot(mean_pred_gp_effects, group_effects[names(mean_pred_gp_effects)])
# => mean pred. effect concentrated at 0 due to partial pooling? 

###############################################################################
# Real data fit 

real_data_stan_fit <- sampling(object = stan_program, 
                               data = list(n_observations = nrow(training_data), 
                                           response = training_data$log_price - mean(training_data$log_price), 
                                           n_explanatory_vars = ncol(scaled_data), 
                                           design_matrix_X = as.matrix(scaled_data),
                                           
                                           # GP parameters (restricted in 'in-sample' post codes)
                                           n_groups = nrow(sq_distance_matrix), 
                                           sq_distance_matrix = as.matrix(sq_distance_matrix), 
                                           group_indicator = group_indicator
                                           # ,
                                           # 
                                           # # distances for extrapolating intercepts for OOS post codes
                                           # n_all_post_codes = nrow(reformed_distance_matrix), 
                                           # sq_distance_matrix_all_post_codes = reformed_distance_matrix
                               ),
                               cores = 4, 
                               iter = 2000)
# save(real_data_stan_fit, file = "real_data_fit.RData")

print(real_data_stan_fit, pars = "beta_coeffs")
# print(fake_data_stan_fit, pars = "intercept"); intercept_draw
print(real_data_stan_fit, pars = c("rho", "alpha", "sigma_gp"))
print(real_data_stan_fit, pars = "sigma_resid")

posterior_sample_real_data <- as.matrix(real_data_stan_fit)
group_effects_posterior_sample <- posterior_sample_real_data[, grep("gen_quan", colnames(posterior_sample_real_data))]


library(doParallel)
library(foreach)

registerDoParallel(10)
OOS_gp_effects_pred_sample <- foreach (k = 1:nrow(posterior_sample_real_data), .combine = rbind) %dopar% {
  generate_OOS_gp_effects(k, posterior_sample = posterior_sample_real_data)
}
stopImplicitCluster()
save.image("./git/simple_gp_model_for_housing_prices_w_post_codes/gp intercepts/mean_pred_gp_effects_done.RData")

mean_out_of_sample_gp_intercepts <- colMeans(OOS_gp_effects_pred_sample)
mean_in_sample_gp_intercepts <- colMeans(group_effects_posterior_sample)
names(mean_in_sample_gp_intercepts) <- sapply(1:nrow(sq_distance_matrix), function(k) indicator_rev_hash[[as.character(k)]]) 

###############################################################################
# Prediction for the in-sample and out-of-sample data 

in_sample_test_data <- read.csv2("./git/simple_gp_model_for_housing_prices_w_post_codes/data/in_sample_test_data.csv", 
                                 stringsAsFactors = F)
out_of_sample_test_data <- read.csv2("./git/simple_gp_model_for_housing_prices_w_post_codes/data/geographical_test_data.csv", 
                                     stringsAsFactors = F) 

in_sample_test_data$padded_post_code <- sapply(in_sample_test_data$post_code, 
                                               pad_postcode)
out_of_sample_test_data$padded_post_code <- sapply(out_of_sample_test_data$post_code, 
                                                   pad_postcode)


# post code "46860" moved to OOS - some problem with the data construction?
out_of_sample_test_data <- rbind(out_of_sample_test_data, 
                                 in_sample_test_data[in_sample_test_data$padded_post_code == "46860",])
in_sample_test_data <- in_sample_test_data[in_sample_test_data$padded_post_code != "46860",]

IS_test_data_scaled_var_list <- lapply(X = explanatory_var_names, 
                                       FUN = center_and_scale_variables,
                                       data = in_sample_test_data, 
                                       input_scaling_parameters = scaling_parameters)

scaled_data_IS_test_data <- data.frame(do.call(cbind, IS_test_data_scaled_var_list))
names(scaled_data_IS_test_data) <- paste(explanatory_var_names, "_stdized", sep = "")

in_sample_test_data <- cbind(in_sample_test_data, scaled_data_IS_test_data)

OOS_test_data_scaled_var_list <- lapply(X = explanatory_var_names, 
                                        FUN = center_and_scale_variables,
                                        data = out_of_sample_test_data, 
                                        input_scaling_parameters = scaling_parameters)

scaled_data_OOS_test_data <- data.frame(do.call(cbind, OOS_test_data_scaled_var_list))
names(scaled_data_OOS_test_data) <- paste(explanatory_var_names, "_stdized", sep = "")

out_of_sample_test_data <- cbind(out_of_sample_test_data, scaled_data_OOS_test_data)

# predictions for in-sample data

gp_intercepts_for_IS_post_codes <- posterior_sample_real_data[, grep("GP_group_effects_gen_quan", colnames(posterior_sample_real_data))]
colnames(gp_intercepts_for_IS_post_codes) <- sapply(1:ncol(gp_intercepts_for_IS_post_codes), 
                                                    function(k) indicator_rev_hash[[as.character(k)]])

beta_vec_posterior_sample <- posterior_sample_real_data[, grep("beta_coeffs", colnames(posterior_sample_real_data))]

in_sample_pred_EV <- as.matrix(scaled_data_IS_test_data) %*% t(beta_vec_posterior_sample)

get_intercept_samples <- function(input_post_code, intercepts_sample) {
  intercepts_sample[, input_post_code]
}

# transposes applied s.t. each row represents a single draw for the EV 
in_sample_pred_EV <- t(in_sample_pred_EV) + 
  sapply(in_sample_test_data$padded_post_code, function(x) get_intercept_samples(x, gp_intercepts_for_IS_post_codes))

pred_dist_sigma <- posterior_sample_real_data[, "sigma_resid"]

in_sample_pred_dist_draws <- apply(in_sample_pred_EV, 
                                   MARGIN = 2, 
                                   function(column) rnorm(length(column), 
                                                          mean = column, 
                                                          sd = pred_dist_sigma))

in_sample_pred_dist_draws <- in_sample_pred_dist_draws + mean(training_data$log_price)

plot(log(in_sample_test_data$price), colMeans(in_sample_pred_dist_draws))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')


plot(in_sample_test_data$price, colMeans(exp(in_sample_pred_dist_draws)))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

k <- 935
in_sample_test_data[k, ]
hist(exp(in_sample_pred_dist_draws[, k]))
abline(v = in_sample_test_data$price[k], col = 'red', lty = 2, lwd = 2)

# predictions for the out-of-sample data 

out_of_sample_pred_EV <- as.matrix(scaled_data_OOS_test_data) %*% t(beta_vec_posterior_sample)

out_of_sample_pred_EV <- t(out_of_sample_pred_EV) + 
  sapply(out_of_sample_test_data$padded_post_code, function(x) get_intercept_samples(x, OOS_gp_effects_pred_sample))

out_of_sample_pred_dist_draws <- apply(out_of_sample_pred_EV, 
                                       MARGIN = 2, 
                                       function(column) rnorm(length(column), 
                                                              mean = column, 
                                                              sd = pred_dist_sigma))

out_of_sample_pred_dist_draws <- out_of_sample_pred_dist_draws + mean(training_data$log_price)
 
plot(log(out_of_sample_test_data$price), colMeans(out_of_sample_pred_dist_draws))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

# really strange predictions for a single observation... 
problem_obs_index <- which.max(abs(log(out_of_sample_test_data$price) - colMeans(out_of_sample_pred_dist_draws)))

out_of_sample_test_data[1017, ]
#   => size of 3785 m^2 in Kemi?!

plot(log(out_of_sample_test_data$price[-c(1014, 1017)]), colMeans(out_of_sample_pred_dist_draws[, -c(1014, 1017)]))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

plot(colMeans(out_of_sample_pred_dist_draws[, -c(1014, 1017)]), log(out_of_sample_test_data$price[-c(1014, 1017)]))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

plot(colMeans(exp(out_of_sample_pred_dist_draws[, -c(1014, 1017)])), out_of_sample_test_data$price[-c(1014, 1017)])
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

# point predictions for description 

test_set_pred_dist_list_gp <- list(in_sample_pred_dist_draws = in_sample_pred_dist_draws,
                                   out_of_sample_pred_dist_draws = out_of_sample_pred_dist_draws)

save(test_set_pred_dist_list_gp, 
     file = "./git/simple_gp_model_for_housing_prices_w_post_codes/gp intercepts/test_set_pred_dist_list_gp.RData")

###############################################################################
# LOO stuff for description

library(loo)
options(mc.cores = 4)

loo_fit <- loo(real_data_stan_fit)
summary(loo_fit)
pareto_k_table(loo_fit)

  # => 17 problematic observations (should use student's t-distribution likelihood?)

utils::View(training_data[pareto_k_ids(loo_fit, threshold = 0.7), ])

loo_fit_gp <- loo_fit
save(loo_fit_gp, file = "./git/simple_gp_model_for_housing_prices_w_post_codes/gp intercepts/loo_git_gp.RData")

###############################################################################
# training data PIT stuff for description

training_data_sample_pred_EV <- as.matrix(scaled_data) %*% t(beta_vec_posterior_sample)

# transposes applied s.t. each row represents a single draw for the EV 
training_data_sample_pred_EV <- t(training_data_sample_pred_EV) + 
  sapply(as.character(training_data$padded_post_code), function(x) get_intercept_samples(x, gp_intercepts_for_IS_post_codes))

pred_dist_sigma <- posterior_sample_real_data[, "sigma_resid"]

training_data_pred_dist_draws <- apply(training_data_sample_pred_EV, 
                                   MARGIN = 2, 
                                   function(column) rnorm(length(column), 
                                                          mean = column, 
                                                          sd = pred_dist_sigma))

training_data_pred_dist_draws <- training_data_pred_dist_draws + mean(training_data$log_price)

plot(training_data$log_price, colMeans(training_data_pred_dist_draws))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

training_data_PIT_transform <- sapply(1:nrow(training_data), 
                                      function(k) {
                                        target_log_price <- training_data$log_price[k]
                                        predictive_distribution <- training_data_pred_dist_draws[, k]
                                        
                                        mean(predictive_distribution <= target_log_price)
                                      })

hist(training_data_PIT_transform, 
     probability = T)
abline(h = 1, lty = 2)
 # => not well-calibrated 

training_data_PIT_transform_gp <- training_data_PIT_transform
save(training_data_PIT_transform_gp, 
     file = "./git/simple_gp_model_for_housing_prices_w_post_codes/gp intercepts/PIT_data_gp.RData")

###############################################################################
# Posterior estimates etc for description

library(xtable)
library(dplyr)
library(rstan)

summary(real_data_stan_fit, pars = c("beta_coeffs", "sigma_resid"))$summary %>% 
  xtable(caption = "Coefficient estimates")

# print(fake_data_stan_fit, pars = "intercept"); intercept_draw
summary(real_data_stan_fit, pars = c("rho", "alpha", "sigma_gp"))$summary %>% 
  xtable(caption = "GP model parameter estimates")

###############################################################################
# boxplots for the post codes 

# in-sample post codes

post_code_effects_gp <- group_effects_posterior_sample

colnames(post_code_effects_gp) <- sapply(as.character(1:ncol(post_code_effects_gp)), function(x) indicator_rev_hash[[x]])

highest_post_code_effects <- post_code_effects_gp[, head(order(colMeans(post_code_effects_gp), decreasing = T), 30)]
lowest_post_code_effects <- post_code_effects_gp[, head(order(colMeans(post_code_effects_gp), decreasing = F), 30)]

boxplot(highest_post_code_effects[, ncol(highest_post_code_effects):1], 
        horizontal = T, 
        outline = F, 
        axes = F,
        ylim = range(highest_post_code_effects)
        )

axis(side = 1, 
     round(seq(from = min(highest_post_code_effects), to = max(highest_post_code_effects), length.out = 8), digits = 2))

axis(side = 2, 
     at = ncol(highest_post_code_effects):1, 
     labels = colnames(highest_post_code_effects),
     las = 2)

abline(h = ncol(highest_post_code_effects):1, lty = 2, col = 'lightgrey', lwd = 0.75)

boxplot(lowest_post_code_effects, 
        horizontal = T, 
        outline = F,
        axes = F, 
        ylim = range(lowest_post_code_effects))

axis(side = 1, 
     round(seq(from = min(lowest_post_code_effects), to = max(lowest_post_code_effects), length.out = 8), digits = 2))

axis(side = 2, 
     at = 1:ncol(lowest_post_code_effects), 
     labels = colnames(lowest_post_code_effects),
     las = 2)
abline(h = 1:ncol(lowest_post_code_effects), lty = 2, col = 'lightgrey', lwd = 0.75)

save(post_code_effects_gp, 
     file = "./git/simple_gp_model_for_housing_prices_w_post_codes/gp intercepts/post_code_effects_gp.RData")

save(OOS_gp_effects_pred_sample, 
     file = "./git/simple_gp_model_for_housing_prices_w_post_codes/gp intercepts/OOS_post_code_effects_gp.RData")



###############################################################################
# Prediction function for new data for Shiny app


post_code_effects <- cbind(gp_intercepts_for_IS_post_codes, 
                           OOS_gp_effects_pred_sample)

get_predictive_distribution_sample <- function(input_data, 
                                               explanatory_var_names_vector = explanatory_var_names, 
                                               scaling_parameters_list = scaling_parameters, 
                                               beta_vec_posterior_sample_matrix = beta_vec_posterior_sample, 
                                               post_code_effects_matrix = post_code_effects, 
                                               residual_stdev_posterior_sample_vector = posterior_sample_real_data[, "sigma_resid"],
                                               mean_training_set_log_price_constant = mean(training_data$log_price),
                                               seed_for_sampling = 123) {
  
  center_and_scale_variables <- function(var_name, data, input_scaling_parameters) {
    var_mean <- input_scaling_parameters[[var_name]][1]
    var_sd <- input_scaling_parameters[[var_name]][2]
    
    result_var <- (data[[var_name]] - var_mean)/(2*var_sd)
    
    return(result_var)
  }
  
  get_intercept_samples <- function(input_post_code, intercepts_sample) {
    intercepts_sample[, input_post_code]
  }
  
  # scale inputs
  scaled_input_data_list <- lapply(X = explanatory_var_names_vector, 
                                   FUN = center_and_scale_variables,
                                   data = input_data, 
                                   input_scaling_parameters = scaling_parameters_list)
  
  scaled_input_data <- data.frame(do.call(cbind, scaled_input_data_list))
  
  # calculate EV 
  EV <- t(as.matrix(scaled_input_data) %*% t(beta_vec_posterior_sample_matrix)) + 
    sapply(input_data$padded_post_code, function(x) get_intercept_samples(x, post_code_effects_matrix))
  
  # sample from likelihood distribution 
  set.seed(seed_for_sampling)
  
  predictive_distribution_sample <- apply(EV, 
                                          MARGIN = 2, 
                                          function(column) rnorm(length(column), 
                                                                 mean = column, 
                                                                 sd = residual_stdev_posterior_sample_vector))
  
  predictive_distribution_sample <- predictive_distribution_sample + mean_training_set_log_price_constant
  
  predictive_distribution_sample
}

input_data <- data.frame(square_meters = 75, 
                         own_property_dummy = 1,
                         row_house_dummy = 0,
                         town_house_dummy = 0,
                         sauna_dummy = 1,
                         condition_unrecorded_dummy = 0,
                         condition_good_dummy = 1,
                         condition_adequate_dummy = 0,
                         age_of_building = 2020 - 2001, 
                         padded_post_code = "02600", 
                         stringsAsFactors = F)

pred_dist_sample <- get_predictive_distribution_sample(input_data = input_data)

pred_dist_sample %>% exp %>% hist
pred_dist_sample %>% exp %>% summary

# components needed for prediction 

prediction_components <- list(explanatory_var_names_vector = explanatory_var_names, 
                              scaling_parameters_list = scaling_parameters, 
                              beta_vec_posterior_sample_matrix = beta_vec_posterior_sample, 
                              post_code_effects_matrix = post_code_effects, 
                              residual_stdev_posterior_sample_vector = posterior_sample_real_data[, "sigma_resid"],
                              mean_training_set_log_price_constant = mean(training_data$log_price), 
                              
                              get_predictive_distribution_sample = get_predictive_distribution_sample)

saveRDS(object = prediction_components, 
        file = "./git/simple_gp_model_for_housing_prices_w_post_codes/gp predictions shiny app/components.RData")
