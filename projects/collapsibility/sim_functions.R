library(dplyr)
library(ggplot2)

# Simulation functions -------
simulate_predictors <- function(n,sigma,x_threshold){
  mvtnorm::rmvnorm(
    n = n,
    sigma = sigma
  ) %>%
    as.data.frame() %>%
    setNames(c("W","X","Z")) %>%
    mutate(
      X = if_else(X > x_threshold,1,0)
    )
}

simulate_linear_model <- function(n,sigma,y_formula,x_threshold,epsilon_sd){
  simulate_predictors(
    n = n,
    sigma = sigma,
    x_threshold = x_threshold
  ) %>%
    mutate(
      epsilon = rnorm(n = n,sd = epsilon_sd),
      Y = {{y_formula}} + epsilon,
    )
}

simulate_logistic_reg_model <- function(n,sigma,y_formula,x_threshold){
  simulate_predictors(
    n = n,
    sigma = sigma,
    x_threshold = x_threshold
  ) %>%
    mutate(
      linear_combination = {{y_formula}},
      Y_probs = plogis(linear_combination),
      outcome = rbinom(n = n(),size = 1,prob = Y_probs)
    )
}

simulate_multiple_datasets <- function(n,sigma,n_simulations,simulate_function,...){
  is_cov_matrix_valid <- all(eigen(sigma)$values != 0)
  stopifnot("Error: supplied covariance matrix must be nonsingular" = is_cov_matrix_valid)

  purrr::map(
    .x = seq(1,n_simulations),
    .f = \(i) simulate_function(
      n = n,
      sigma = sigma,
      ...
    ) %>%
      mutate(sim_id = i)
  ) %>%
    # Bind into dataframe and nest under sim_id so we can perform calculations for
    # each simulation easily
    bind_rows() %>%
    relocate(sim_id) %>%
    group_by(sim_id) %>%
    nest()
}

get_coef_df <- function(sim_results,true_coef_df){
  sim_results %>%
    mutate(
      betas = map(fitted_model, ~ coef(.x))
    ) %>%
    select(sim_id,betas) %>%
    unnest_wider(betas) %>%
    pivot_longer(cols = c(everything(), -sim_id),names_to = "parameter",values_to = "estimate") %>%
    left_join(
      true_coef_df,
      by = "parameter"
    )
}

# Presentation helpers -----

plot_coefficient_distribution <- function(coef_df,title){
  parameter_string = coef_df %>%
    ungroup() %>%
    distinct(parameter,true_value) %>%
    mutate(pair = glue::glue("{parameter} = {true_value}")) %>%
    pull(pair) %>%
    paste(collapse = ", ")

  coef_df %>%
    ggplot(aes(x = estimate,y = parameter,fill = parameter)) +
    #geom_boxplot(outliers = FALSE) +
    geom_violin(draw_quantiles = c(.5),) +
    geom_point(aes(x = true_value,color = "True value of parameter"),shape = 23,fill = "red",size = 3) +
    scale_color_manual(name = "",values = c("True value of parameter" = "red")) +
    scale_fill_brewer(name = "Parameter estimates for",type = "qual",palette = 3) +
    scale_x_continuous(name = "Parameter estimates") +
    labs(
      y = "Variables",
      title = title,
      subtitle = glue::glue("True parameter values are {parameter_string}")
    ) +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2)
    )
}

summarize_coef_df <- function(coef_df,caption){
  coef_df %>%
    group_by(parameter) %>%
    rename(variable = parameter) %>%
    summarize(
      parameter_estimate = mean(estimate),
      true_value = true_value[1],
      bias = mean(estimate - true_value)
    ) %>%
    janitor::clean_names(case = "sentence") %>%
    knitr::kable(caption = caption)
}
