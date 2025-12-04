# Libraries----

# Loading the libraries and installing them if not in the Rproj

local({
  pkgs <- c("targets", "tidyverse", "janitor", "brms","broom.mixed", "emmeans", "glmmTMB", "DHARMa", "ggtext", "conflicted")
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) install.packages(missing)
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
})

conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  brms::ar,
  brms::lognormal,
  stats::chisq.test,
  stats::fisher.test
)

options(scipen = 999)



# Data cleaning----

data_tidying <- function(data) {
  data_tidy <- data %>%
    clean_names() %>%
    relocate(year) %>% 
    rename(any_of(c(summit = "top", elevation = "height", elevation = "top_height", weather = "vaer"))) %>%
    mutate(summit = str_replace_all(summit, " ", "_"),
           across(any_of("date"), ~ dmy(.x)),
           across(any_of("weather"), ~ str_replace_all(.x, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_"))),
           across(any_of("recorder"), ~ str_replace_all(.x, c(" \\+ " = "_", "\\+" = "_"))),
           species = str_replace_all(species, c(" " = "_", "\\." = ""))) %>%
    arrange(desc(elevation), species)
  return(data_tidy)
}


# Plotting----

gg_yearline <- function(data, y_var, x_var, row_var, col_var, colour_var) {
  data |> 
    summarise(.by = c({{x_var}}, {{colour_var}}, {{row_var}}, {{col_var}}), 
              mean = mean(.data[[y_var]], na.rm = TRUE)) |> 
    ggplot(aes(x = .data[[x_var]], y = mean, colour = .data[[colour_var]])) + 
    geom_line(size = 2) + 
    scale_colour_manual(values = colour_mapping[[as.character(substitute(colour_var))]]) + 
    facet_grid(rows = vars(.data[[row_var]]), cols = vars(.data[[col_var]]), labeller = adj_label) + 
    scale_x_continuous(n.breaks = 4) + 
    theme_test() + 
    theme(legend.position = "top")
}

gg_modvars <- function(data, y_var, x_var, col_var = NULL, row_var = NULL) {
  plot <- data |> 
    filter(!is.na(.data[[y_var]])) |> 
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) + 
    geom_jitter(width = 0.2, alpha = 0.2) + 
    geom_boxplot(alpha = 0.8) + 
    scale_fill_manual(values = colour_mapping[[x_var]]) + 
    scale_x_discrete(labels = adj_label) + 
    theme_test() + 
    geom_hline(yintercept = 0, colour = "black") + 
    theme(legend.position = "none")
  if (!is.null(col_var) && !is.null(row_var)) {
    plot <- plot + facet_grid(rows = vars(.data[[row_var]]), cols = vars(.data[[col_var]]), labeller = adj_label)
  } else if (!is.null(col_var)) {
    plot <- plot + facet_grid(cols = vars(.data[[col_var]]), labeller = adj_label)
  }
  return(plot)
}

adj_label <- c("richness" = "<b>a)</b> Species<br>richness", 
               "new" = "<b>b)</b> New species", 
               "lost" = "<b>c)</b> Lost species", 
               "elevation" = "<b>d)</b> Species<br>altitude",
               "alpine" = "Alpine",
               "generalist" = "Generalist")

colour_mapping <-  list(
  period = c("period1" = "#859395", "period2" = "#f58800"),
  category = c("alpine" = "#859395", "generalist" = "#f58800")
)


# Model results----





# Model fitness----

model_diagnosis <- function(fitted_model, timev = NULL) {
  sim_res <- simulateResiduals(fitted_model, plot = FALSE)
  plot.new()
  
  # Perform the diagnostic tests
  uniformity_test <- testUniformity(sim_res, plot = FALSE)
  outliers_test <- testOutliers(sim_res, plot = FALSE)
  dispersion_test <- testDispersion(sim_res, plot = FALSE)
  quantiles_test <- testQuantiles(sim_res, plot = FALSE) # Note: running plot = FALSE changes the methods used in the test, which may change the results
  zero_inflation_test <- testZeroInflation(sim_res, plot = FALSE)
  
  # Collect the results in a list
  results <- list(
    UNIFORMITY = uniformity_test,
    OUTLIERS = outliers_test,
    DISPERSION = dispersion_test,
    QUANTILES = quantiles_test,
    ZERO_INFLATION = zero_inflation_test
  )
  
  # In case we have a temporal variable and want to check for autocorrelation
  if (!is.null(timev)) {
    time_vector <- fitted_model$frame |> pull({{timev}})
    test_temporal_autocorrelation <- testTemporalAutocorrelation(recalculateResiduals(sim_res, group = time_vector), time = unique(time_vector), plot = FALSE)
    results$TEMPORAL_AUTOCORRELATION <- test_temporal_autocorrelation
  }
  
  # This function removes the console output (used when creating the plots)
  in_silence <- function(...) {
    mc <- match.call()[-1]
    a <- capture.output(
      tryCatch(
        suppressMessages(suppressWarnings(
          eval(as.list(mc)[[1]])
        )), error = function(e) ""))
  }
  
  # Generate and display the plots
  par(mfrow = c(2, 2))
  in_silence(testUniformity(sim_res))
  in_silence(testOutliers(sim_res))
  in_silence(testDispersion(sim_res))
  in_silence(testQuantiles(sim_res))
  par(mfrow = c(1, 1))
  
  return(results)
}

model_homoscedasticity <- function(fitted_model) {
  sim_res <- simulateResiduals(fitted_model, plot = FALSE)
  response_variable <- all.vars(formula(fitted_model))[1]
  explanatory_variables <- fitted_model |> formula() |> terms() |> attr("term.labels")
  single_variables <- explanatory_variables[!grepl("[:/(|]", explanatory_variables)]
  plot.new() # Initialize a new plot
  
  # Perform the diagnostic tests
  test_statistics <- lapply(single_variables, function(var) {
    testCategorical(sim_res, catPred = fitted_model$frame %>% filter(!is.na(response_variable)) %>% pull(var), plot = FALSE)
  })
  
  names(test_statistics) <- single_variables
  
  # This function removes the console output (used when creating the plots)
  in_silence <- function(...) {
    mc <- match.call()[-1]
    a <- capture.output(
      tryCatch(
        suppressMessages(suppressWarnings(
          eval(as.list(mc)[[1]])
        )), error = function(e) ""))
  }
  
  # Generate and display the plots
  par(mfrow = c(2, 2))
  for (var in single_variables) {
    in_silence(testCategorical(sim_res, catPred = fitted_model$frame %>% filter(!is.na(response_variable)) %>% pull(var)))
  }
  par(mfrow = c(1, 1))
  
  # Formatting the output
  format_testCategorical_output <- function(test_result) {
    p_values <- test_result$uniformity$p.value
    p_values_cor <- test_result$uniformity$p.value.cor
    homogeneity <- test_result$homogeneity
    results <- list(p_values = p_values, Corrected_p_values = p_values_cor, Homogeneity = homogeneity)
    return(results)
  }
  formatted_output <- lapply(test_statistics, format_testCategorical_output)
  
  return(formatted_output)
}
