
# Simple hierarchical model, normal likelihood  
# 31.1.2021
# Ville Mäkinen

# Simple hierarchical model to illustrate poor generalization 

setwd("/home/asdf/Desktop/asuntojen hinnat/")
rm(list = ls())
gc()

training_data <- read.csv2("./git/simple linear regression normal dist/data/training_data.csv")
# map_data <- read.csv("./git/simple linear regression normal dist/post codes map/post_code_centroids.csv")

###############################################################################
# Data preparation
training_data$log_price <- log(training_data$price)

training_data$post_code <- as.character(training_data$post_code)

pad_postcode <- function(x) {
  padding_length <- 5 - nchar(x)
  
  padding <- paste(rep("0", padding_length), collapse="")
  return(paste(padding, x, collapse = "", sep = ""))
}

training_data$padded_post_code <- sapply(training_data$post_code, pad_postcode)

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

post_code_muni_mapping_df <- read.csv2("./paavo aineisto/post_code_muni_name_mapping.csv", 
                                       stringsAsFactors = F)

library(dplyr)
all_post_codes <- post_code_muni_mapping_df$post_code %>% sapply(pad_postcode) %>% unique

training_data$padded_post_code <- factor(training_data$padded_post_code, levels = all_post_codes)

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

# sample draws for the post code population 
# mu_pop <- rnorm(1) 

mu_pop <- 0 

library(extraDistr)
sigma_pop <- rhcauchy(1)

post_code_intercepts <- rnorm(n = length(levels(training_data$padded_post_code)), 
                              mean = mu_pop, 
                              sd = sigma_pop)
# hist(post_code_intercepts)

group_effects <- post_code_intercepts[as.numeric(training_data$padded_post_code)]

beta_vec <- matrix(c(square_meters_coef_draw,
                     own_property_dummy_coef_draw,
                     row_house_dummy_coef_draw,
                     town_house_dummy_coef_draw,
                     sauna_dummy_coef_draw,
                     condition_unrecorded_dummy_coef_draw,
                     condition_good_dummy_coef_draw,
                     condition_adequate_dummy_coef_draw,
                     age_of_building_coef_draw), ncol = 1)


EV_fake_data <- intercept_draw + group_effects + as.matrix(scaled_data) %*% beta_vec 
EV_fake_data <- as.numeric(EV_fake_data)

sigma_draw <- rhcauchy(1)

fake_log_price_draw <- EV_fake_data + sigma_draw*rnorm(n = length(EV_fake_data))

fake_price_draw <- exp(fake_log_price_draw)

# visual check whether chosen priors produce 'reasonable' prices
graphing_breaks <- quantile(fake_price_draw, probs = c(0.05, 0.95))

hist(fake_price_draw[fake_price_draw > graphing_breaks[1] & 
                       fake_price_draw < graphing_breaks[2]], nclass = 100)

group_indicator <- as.numeric(factor(as.numeric(training_data$padded_post_code)))

###############################################################################
# Fake data fit 

library(rstan)

# stan_program <- stan_model(file = "./simple linear regression normal dist/simple_linear_reg_normal_likelihood_stan_prog.stan")
stan_program <- stan_model(file = "./git/simple_gp_model_for_housing_prices_w_post_codes/simple linear regression normal dist/simple_linear_reg_normal_likelihood_no_intercept_stan_prog.stan")

fake_data_stan_fit <- sampling(object = stan_program, 
                               data = list(n_observations = nrow(training_data), 
                                           # response = as.numeric(fake_log_price_draw),
                                           response = as.numeric(fake_log_price_draw) - mean(as.numeric(fake_log_price_draw)),
                                           n_explanatory_vars = ncol(scaled_data), 
                                           design_matrix_X = as.matrix(scaled_data),
                                           
                                           # group effect information
                                           n_groups = max(group_indicator), 
                                           group_indicator = group_indicator),
                               cores = 4, 
                               iter = 4000)
# save(fake_data_stan_fit, file = "./git/simple_gp_model_for_housing_prices_w_post_codes/simple linear regression normal dist/fake_data_fit.RData")

print(fake_data_stan_fit)

print(fake_data_stan_fit, pars = "beta_coeffs"); beta_vec
plot(fake_data_stan_fit, pars = "beta_coeffs")

# print(fake_data_stan_fit, pars = "intercept"); intercept_draw
print(fake_data_stan_fit, pars = "sigma_resid"); sigma_draw
plot(fake_data_stan_fit, pars = "sigma_resid")

# print(fake_data_stan_fit, pars = "mu_pop"); mu_pop
print(fake_data_stan_fit, pars = "sigma_pop"); sigma_pop

posterior_sample_fake_data <- as.matrix(fake_data_stan_fit)
group_effects_posterior_sample <- posterior_sample_fake_data[, grep("gen_quan", colnames(posterior_sample_fake_data))]


# group_effects <- post_code_intercepts[as.numeric(training_data$padded_post_code)]
# 


limited_intercept_draws <- post_code_intercepts[sort(unique(as.numeric(training_data$padded_post_code)))] 

k <- 1
hist(group_effects_posterior_sample[, k])
abline(v = limited_intercept_draws[k], col = 'red', lty = 2, lwd = 2)

###############################################################################
# Real data fit 

real_data_stan_fit <- sampling(object = stan_program, 
                               data = list(n_observations = nrow(training_data), 
                                           # response = as.numeric(fake_log_price_draw),
                                           response = training_data$log_price - mean(training_data$log_price),
                                           n_explanatory_vars = ncol(scaled_data), 
                                           design_matrix_X = as.matrix(scaled_data),
                                           
                                           # group effect information
                                           n_groups = max(group_indicator), 
                                           group_indicator = group_indicator),
                               cores = 4, 
                               iter = 6000)
print(real_data_stan_fit)

print(real_data_stan_fit, pars = "beta_coeffs")
explanatory_var_names

# save.image("real_data_fit.RData")

posterior_sample_real_data <- as.matrix(real_data_stan_fit)
group_effects_posterior_sample <- posterior_sample_real_data[, grep("gen_quan", colnames(posterior_sample_real_data))]

library(hash)

# hash for translating the subset of the post codes found in the estimation set

hash_df <- data.frame(group_indicator, 
                      post_code = as.character(training_data$padded_post_code))

hash_df <- unique(hash_df)

indicator_to_post_code_hash <- hash(keys = as.character(hash_df$group_indicator),
                                    values = hash_df$post_code)

###############################################################################
# Prediction for the in-sample and out-of-sample data 

in_sample_test_data <- read.csv2("./git/simple_gp_model_for_housing_prices_w_post_codes/data/in_sample_test_data.csv", 
                                 stringsAsFactors = F)

in_sample_test_data$padded_post_code <- sapply(in_sample_test_data$post_code, 
                                               pad_postcode)

out_of_sample_test_data <- read.csv2("./git/simple_gp_model_for_housing_prices_w_post_codes/data/geographical_test_data.csv", 
                                     stringsAsFactors = F) 

out_of_sample_test_data$padded_post_code <- sapply(out_of_sample_test_data$post_code, 
                                                   pad_postcode)

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

beta_vec_posterior_sample <- posterior_sample_real_data[, grep("beta_coeffs", colnames(posterior_sample_real_data))]
known_post_code_effects <- posterior_sample_real_data[, grep("gen_quan", colnames(posterior_sample_real_data))]
pred_dist_sigma <- posterior_sample_real_data[, "sigma_resid"]

group_indicator_hash <- hash(keys = as.character(training_data$padded_post_code), 
                             values = group_indicator)

in_sample_test_data$group_id <- sapply(in_sample_test_data$padded_post_code, 
                                       function(x) group_indicator_hash[[x]])

in_sample_pred_EV <- as.matrix(scaled_data_IS_test_data) %*% t(beta_vec_posterior_sample)

in_sample_pred_EV <- t(in_sample_pred_EV) + known_post_code_effects[, in_sample_test_data$group_id]

in_sample_pred_dist_draws <- apply(in_sample_pred_EV, 
                                   MARGIN = 2, 
                                   function(column) rnorm(length(column), 
                                                          mean = column, 
                                                          sd = pred_dist_sigma))

translated_IS_pred_dist <- in_sample_pred_dist_draws + mean(training_data$log_price)
translated_IS_pred_dist <- exp(translated_IS_pred_dist)

plot(colMeans(translated_IS_pred_dist), in_sample_test_data$price)
abline(a = 0, b= 1, col = 'red', lty = 2, lwd = 2)

which.max(abs(colMeans(translated_IS_pred_dist) - in_sample_test_data$price))
# large overprediction 

k <- 935
in_sample_test_data[k, ]
hist(translated_IS_pred_dist[, k])
abline(v = in_sample_test_data$price[k], col = 'red', lty = 2, lwd = 2)

# plots for the description 

# predictions for the out-of-sample data --------------------------------------

sum(out_of_sample_test_data$padded_post_code %in% training_data$padded_post_code)

out_of_sample_pred_EV <- as.matrix(scaled_data_OOS_test_data) %*% t(beta_vec_posterior_sample)
out_of_sample_pred_EV <- t(out_of_sample_pred_EV)

# no known post codes in the out of sample => need to generate synthetic 
# equivalents... 

posterior_sample_pop_sigma <- posterior_sample_real_data[, "sigma_pop"]

post_code_effects <- sapply(1:nrow(out_of_sample_test_data), 
                            function(k) rnorm(n = length(posterior_sample_pop_sigma), 
                                              mean = 0, 
                                              sd = posterior_sample_pop_sigma))

out_of_sample_pred_EV <- out_of_sample_pred_EV + post_code_effects

out_of_sample_pred_dist_draws <- apply(out_of_sample_pred_EV, 
                                       MARGIN = 2, 
                                       function(column) rnorm(length(column), 
                                                              mean = column, 
                                                              sd = pred_dist_sigma))

translated_OOS_pred_dist <- out_of_sample_pred_dist_draws + mean(training_data$log_price)
translated_OOS_pred_dist <- exp(translated_OOS_pred_dist)

plot(colMeans(translated_OOS_pred_dist), out_of_sample_test_data$price)
abline(a = 0, b= 1, col = 'red', lty = 2, lwd = 2)

which.max(abs(colMeans(translated_OOS_pred_dist) - out_of_sample_test_data$price))


plot(colMeans(translated_OOS_pred_dist[, -1017]), out_of_sample_test_data$price[-1017])
abline(a = 0, b= 1, col = 'red', lty = 2, lwd = 2)


order(abs(colMeans(translated_OOS_pred_dist) - out_of_sample_test_data$price), decreasing = T)

plot(colMeans(translated_OOS_pred_dist[, -c(1017, 1014)]), out_of_sample_test_data$price[-c(1017, 1014)])
abline(a = 0, b= 1, col = 'red', lty = 2, lwd = 2)


plot(colMeans(exp(out_of_sample_pred_dist_draws[, -c(1014, 1017)]  + mean(training_data$log_price))), out_of_sample_test_data$price[-c(1014, 1017)])
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'red')

# problematic observations 

out_of_sample_test_data[head(order(abs(colMeans(translated_OOS_pred_dist) - out_of_sample_test_data$price), decreasing = T)),]
# => two outliers first (abnormal sizes?!)


# point predictions for description 

test_set_pred_dist_list_simple_lin_reg <- list(in_sample_pred_dist_draws = in_sample_pred_dist_draws + mean(training_data$log_price),
                                               out_of_sample_pred_dist_draws = out_of_sample_pred_dist_draws + mean(training_data$log_price))

save(test_set_pred_dist_list_simple_lin_reg, 
     file = "./git/simple_gp_model_for_housing_prices_w_post_codes/simple linear regression normal dist/test_set_pred_dist_list_simple_lin_reg.RData")

###############################################################################
# LOO stuff for description

library(loo)
options(mc.cores = 4)

loo_fit <- loo(real_data_stan_fit)
print(loo_fit)

utils::View(training_data[pareto_k_ids(loo_fit, threshold = 0.7), ])

loo_fit_simple_lin_reg <- loo_fit
save(loo_fit_simple_lin_reg, file = "./git/simple_gp_model_for_housing_prices_w_post_codes/simple linear regression normal dist/loo_fit_simple_lin_reg.RData")

###############################################################################
# training data PIT stuff for description

training_data_sample_pred_EV <- as.matrix(scaled_data) %*% t(beta_vec_posterior_sample)

# transposes applied s.t. each row represents a single draw for the EV 
training_data_sample_pred_EV <- t(training_data_sample_pred_EV)  + known_post_code_effects[, group_indicator] 

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

training_data_PIT_transform_simple_lin_reg <- training_data_PIT_transform
save(training_data_PIT_transform_simple_lin_reg, file = "./git/simple_gp_model_for_housing_prices_w_post_codes/simple linear regression normal dist/PIT_data_simple_lin_reg.RData")



###############################################################################
# Posterior estimates etc for description

library(xtable)
library(dplyr)
library(rstan)

summary(real_data_stan_fit, pars = c("beta_coeffs", "sigma_resid", "sigma_pop"))$summary %>% 
  xtable(caption = "Simple linear model coefficient estimates")

# post code effects 
post_code_effects_simple_lin_reg <- known_post_code_effects

colnames(post_code_effects_simple_lin_reg) <- sapply(as.character(1:ncol(post_code_effects_simple_lin_reg)), function(x) indicator_to_post_code_hash[[x]])

highest_post_code_effects <- post_code_effects_simple_lin_reg[, head(order(colMeans(post_code_effects_simple_lin_reg), decreasing = T), 30)]
lowest_post_code_effects <- post_code_effects_simple_lin_reg[, head(order(colMeans(post_code_effects_simple_lin_reg), decreasing = F), 30)]


par(mfrow=c(1,2))

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

par(mfrow=c(1,1))

save(post_code_effects_simple_lin_reg, 
     file = "./git/simple_gp_model_for_housing_prices_w_post_codes/simple linear regression normal dist/post_code_effects_simple_lin_reg.RData")


###############################################################################
# => GP model captures spatial effects(?)



