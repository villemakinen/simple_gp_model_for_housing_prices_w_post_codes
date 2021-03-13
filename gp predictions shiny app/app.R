
# Shiny app for price predictions using a simple GP model 
# 6.3.2021
# Ville Mäkinen 

# -----------------------------------------------------------------------------

library(shiny)
library(dplyr)

# loading the components for prediction 

prediction_components <- readRDS("./components.RData")

ui <- fluidPage(
  titlePanel(h1("Price prediction distributions from a simple GP model")),
  br(),
  "6.3.2021",
  
  br(),
  
  "Ville Mäkinen / ville piste ka piste makinen ät gmail piste com",
  
  br(),
  
  "This app provides a quick and dirty way to predict housing prices using a simple gaussian process model estimated using post codes in Finland. No error checking for valid post codes etc. has been implemented.",
  
  br(),
  
  code("Use at your own peril"),
  
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(h2("Inputs"),
                 # input for square_meters
                 numericInput("square_meters", 
                              "Size of the apartment in square meters", 
                              value = 50, 
                              min = 20, 
                              max = 500, 
                              step = 0.5),
                 
                 # input for own_property_dummy
                 strong("Dummy for whether the property is owned by the housing company or rented"),
                 checkboxInput("own_property_dummy", "Property owned by the housing company?"),
                 
                 # input for row_house_dummy, town_house_dummy 
                 radioButtons("type_of_building", 
                              
                              "Type of building",
                              choices = list("Multi-storey building" = 1, 
                                             "Row house" = 2,
                                             "Town house" = 3),
                              selected = 1),
                 
                 
                 # input for sauna_dummy 
                 strong("Dummy for whether the apartment has a sauna"),
                 checkboxInput("sauna_dummy", "Has a sauna"),
                 
                 
                 # input for condition_unrecorded_dummy, condition_good_dummy, condition_adequate_dummy
                 radioButtons("condition", 
                              
                              "Condition of the apartment",
                              choices = list("Not recorded" = 1, 
                                             "Good" = 2,
                                             "Adequate" = 3,
                                             "Bad" = 4),
                              selected = 1),
                 
                 # input for age_of_building
                 numericInput("building_year", 
                              "Year in which the building was built", 
                              value = 2000, 
                              min = 1750, 
                              max = 2050, 
                              step = 1),
                 
                 
                 # input for padded_post_code = "02600"
                 textInput("padded_post_code", "Post code", 
                           value = "00100"),
                 
                 # should this be an action button instead?
                 submitButton("Predict!"),
                 
                 br(), 
                 br(), 
                 "Parameters for graphing",
                 numericInput("rnorm_seed", 
                              "Seed for rnorm for the predictive distribution", 
                              value = 123, 
                              step = 1),
                 numericInput("hist_nclass", 
                              "nclass for the histogram", 
                              value = 20, 
                              min = 1, 
                              max = 1000, 
                              step = 1),
                 numericInput("summary_digits", 
                              "digits for the distribution summary figure rounding", 
                              value = 5, 
                              min = 0, 
                              max = 20, 
                              step = 1)
                 ), 
    mainPanel(h2("Predictive rice distribution"),
              #textOutput("input_data_summary"),
              
              plotOutput("distribution_histogram"),
              
              br(),
              "Predictive distribution summary figures:",
              textOutput("result_summary"),
              br(), 
              
              "Inputs:",
              textOutput("square_meters_text"),  
              textOutput("own_property_dummy_text"), 
              textOutput("row_house_dummy_text"),  
              textOutput("town_house_dummy_text"), 
              textOutput("sauna_dummy_text"),  
              textOutput("condition_unrecorded_dummy_text"),  
              textOutput("condition_good_dummy_text"),
              textOutput("condition_adequate_dummy_text"),
              textOutput("age_of_building_text"),
              textOutput("padded_post_code_text"))
  )
)

server <- function(input, output) {
  
  
  # ugly hack to split the outputs to new lines for textOutput()-calls in the UI
  output$square_meters_text <- renderText({ paste("    square_meters =", input$square_meters) })
  output$own_property_dummy_text <- renderText({ paste("    own_property_dummy =", 1*input$own_property_dummy) })  
  output$row_house_dummy_text <- renderText({ paste("    row_house_dummy =", 1*(input$type_of_building == 2)) })  
  output$town_house_dummy_text <- renderText({ paste("    town_house_dummy =", 1*(input$type_of_building == 3)) })  
  output$sauna_dummy_text <- renderText({ paste("    sauna_dummy =", 1*input$sauna_dummy) })  
  output$condition_unrecorded_dummy_text <- renderText({ paste("    condition_unrecorded_dummy =", 1*(input$condition == 1)) })  
  output$condition_good_dummy_text <- renderText({ paste("    condition_good_dummy =", 1*(input$condition == 2)) })  
  output$condition_adequate_dummy_text <- renderText({ paste("    condition_adequate_dummy =", 1*(input$condition == 3)) })  
  output$age_of_building_text <- renderText({ paste("    age_of_building =", 2020 - input$building_year) })  
  output$padded_post_code_text <- renderText({ paste("    padded_post_code =", input$padded_post_code) })  
  
  output$input_data_summary <- renderText({
  
    square_meters <- input$square_meters
    own_property_dummy <- 1*input$own_property_dummy 
    row_house_dummy <- 1*(input$type_of_building == 2)
    town_house_dummy <- 1*(input$type_of_building == 3)
    sauna_dummy <- 1*input$sauna_dummy
    condition_unrecorded_dummy <- 1*(input$condition == 1)
    condition_good_dummy <- 1*(input$condition == 2)
    condition_adequate_dummy <- 1*(input$condition == 3)
    age_of_building <- 2020 - input$building_year
    padded_post_code <- input$padded_post_code
    
    paste("Inputs: Sqm =",
          square_meters,
          "| own_property_dummy =",
          own_property_dummy,
          "| row_house_dummy =",
          row_house_dummy,
          "| town_house_dummy =",
          town_house_dummy,
          "| sauna_dummy =",
          sauna_dummy,
          "| condition_unrecorded_dummy =",
          condition_unrecorded_dummy,
          "| condition_good_dummy =",
          condition_good_dummy,
          "| condition_adequate_dummy =",
          condition_adequate_dummy,
          "| age_of_building =",
          age_of_building,
          "| padded_post_code =",
          padded_post_code,
          sep = " ")

  })
  
  # predictions are now done stupidly twice: once for the result summary and once for 
  # the graph... 
  
  output$result_summary <- renderText({
  
    input_data <- data.frame(square_meters = input$square_meters, 
                             own_property_dummy = 1*input$own_property_dummy, # "1*" probably unnecessary... 
                             row_house_dummy = 1*(input$type_of_building == 2),
                             town_house_dummy = 1*(input$type_of_building == 3),
                             sauna_dummy = 1*input$sauna_dummy,
                             condition_unrecorded_dummy = 1*(input$condition == 1),
                             condition_good_dummy = 1*(input$condition == 2),
                             condition_adequate_dummy = 1*(input$condition == 3),
                             age_of_building = 2020 - input$building_year, 
                             padded_post_code = input$padded_post_code, 
                             stringsAsFactors = F)
    
    pred_ftion <- prediction_components$get_predictive_distribution_sample
    
    pred_distribution <- pred_ftion(input_data = input_data, 
                                    explanatory_var_names_vector = prediction_components$explanatory_var_names_vector, 
                                    scaling_parameters_list = prediction_components$scaling_parameters_list, 
                                    beta_vec_posterior_sample_matrix = prediction_components$beta_vec_posterior_sample_matrix,
                                    post_code_effects_matrix = prediction_components$post_code_effects_matrix,
                                    residual_stdev_posterior_sample_vector = prediction_components$residual_stdev_posterior_sample_vector,
                                    mean_training_set_log_price_constant = prediction_components$mean_training_set_log_price_constant, 
                                    seed_for_sampling = input$rnorm_seed)
    
    summary_object <- summary(as.numeric(exp(pred_distribution)))  
    
    paste(names(summary_object), round(summary_object, digits = input$summary_digits), sep = ": ")
  })
    
    
  
  
  output$distribution_histogram <- renderPlot({
    
    input_data <- data.frame(square_meters = input$square_meters, 
                             own_property_dummy = 1*input$own_property_dummy, # "1*" probably unnecessary... 
                             row_house_dummy = 1*(input$type_of_building == 2),
                             town_house_dummy = 1*(input$type_of_building == 3),
                             sauna_dummy = 1*input$sauna_dummy,
                             condition_unrecorded_dummy = 1*(input$condition == 1),
                             condition_good_dummy = 1*(input$condition == 2),
                             condition_adequate_dummy = 1*(input$condition == 3),
                             age_of_building = 2020 - input$building_year, 
                             padded_post_code = input$padded_post_code, 
                             stringsAsFactors = F)
    
    pred_ftion <- prediction_components$get_predictive_distribution_sample
    
    pred_distribution <- pred_ftion(input_data = input_data, 
                                    explanatory_var_names_vector = prediction_components$explanatory_var_names_vector, 
                                    scaling_parameters_list = prediction_components$scaling_parameters_list, 
                                    beta_vec_posterior_sample_matrix = prediction_components$beta_vec_posterior_sample_matrix,
                                    post_code_effects_matrix = prediction_components$post_code_effects_matrix,
                                    residual_stdev_posterior_sample_vector = prediction_components$residual_stdev_posterior_sample_vector,
                                    mean_training_set_log_price_constant = prediction_components$mean_training_set_log_price_constant, 
                                    seed_for_sampling = input$rnorm_seed)
    
    pred_distribution %>% exp %>% hist(main = "Predictive distribution sample", nclass = input$hist_nclass)
    
    # abline(v = pred_distribution %>% exp %>% mean, col = 'red', lty = 2, lwd = 3)
    # abline(v = pred_distribution %>% exp %>% median, col = 'darkgreen', lty = 3, lwd = 3)
    abline(v = mean(exp(pred_distribution)) , col = 'red', lty = 2, lwd = 3)
    abline(v = median(exp(pred_distribution)), col = 'darkgreen', lty = 3, lwd = 3)

    
    
    legend("topright", legend = c("Mean", "Median"), col = c("red", "darkgreen"), lty = c(2,3), lwd = 3)
  })
  
}

shinyApp(ui = ui, server = server)

