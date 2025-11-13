# Libraries----

# Loading the libraries and installing them if not in the Rproj

local({
  pkgs <- c("targets", "tidyverse", "janitor", "glmmTMB", "DHARMa", "conflicted")
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) install.packages(missing)
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
})

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
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
           across(any_of("recorder"), ~ str_replace_all(.x, c("\\+", "_", " \\+ ", "_"))),
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

adj_label <- as_labeller(c("new_rate" = "New", "lost_rate" = "Lost", "altitude" = "<b>a)</b> Altitude<br>change", "richness" = "<b>b)</b> Richness<br>change", "new" = "<b>c)</b> Number of<br>new species", "lost" = "<b>d)</b> Number of<br>lost species", "per1" = "1972-2009", "per2" = "2009-2024"))

colour_mapping <-  list(
  period = c("per1" = "#859395", "per2" = "#f58800")
)


# Model selection----

model_distribution <- function(data, variable) {
  
  # Extract the specified column
  column_data <- data[[variable]]
  
  # Fit various distributions with checks for specific requirements
  fit_norm <- fitdist(column_data, "norm")
  fit_lnorm <- if (all(column_data > 0)) fitdist(column_data, "lnorm") else "Not Applicable"
  fit_exp <- if (all(column_data > 0)) fitdist(column_data, "exp") else "Not Applicable"
  fit_pois <- if (all(column_data >= 0) && all(column_data == floor(column_data))) fitdist(column_data, "pois") else "Not Applicable"
  fit_cauchy <- fitdist(column_data, "cauchy")
  fit_gamma <- if (all(column_data > 0)) fitdist(column_data, "gamma") else "Not Applicable"
  fit_logis <- fitdist(column_data, "logis")
  fit_nbinom <- if (all(column_data >= 0) && all(column_data == floor(column_data))) fitdist(column_data, "nbinom") else "Not Applicable"
  fit_geom <- if (all(column_data >= 0) && all(column_data == floor(column_data))) fitdist(column_data, "geom") else "Not Applicable"
  fit_beta <- if (all(column_data > 0 & column_data < 1)) fitdist(column_data, "beta") else "Not Applicable"
  fit_weibull <- if (all(column_data > 0)) fitdist(column_data, "weibull") else "Not Applicable"
  
  # Collect fits in a list
  fits <- list(fit_norm, fit_lnorm, fit_exp, fit_pois, fit_cauchy, fit_gamma, fit_logis, fit_nbinom, fit_geom, fit_beta, fit_weibull)
  
  # Filter out "Not Applicable" fits for gofstat
  applicable_fits <- fits[sapply(fits, function(x) !is.character(x))]
  
  # Calculate goodness-of-fit statistics if there are applicable fits
  if (length(applicable_fits) > 0) {
    gof <- gofstat(applicable_fits)
    print(gof)
  } else {
    print("No applicable distributions found.")
  }
  
  # Print which distributions were not applicable
  dist_names <- c("norm", "lnorm", "exp", "pois", "cauchy", "gamma", "logis", "nbinom", "geom", "beta", "weibull")
  not_applicable <- dist_names[sapply(fits, function(x) is.character(x))]
  if (length(not_applicable) > 0) {
    cat("The following distributions were not applicable:\n")
    print(not_applicable)
  }
}

optimizer <- list(
  glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max=  1e5)), 
  glmmTMBControl(optimizer = "optim", optArgs = list(method = "BFGS")), 
  glmmTMBControl(optimizer = "optim", optArgs = list(method = "L-BFGS-B")), 
  glmmTMBControl(optimizer = "optim", optArgs = list(method = "CG")), 
  glmmTMBControl(optimizer = "nlminb", optArgs = list(method = "BFGS")), 
  glmmTMBControl(optimizer = "nlminb", optArgs = list(method = "L-BFGS-B")), 
  glmmTMBControl(optimizer = "nlminb", optArgs = list(method = "CG"))
)

backwards_selection <- function(fitted_model) {
  original_formula <- formula(fitted_model)
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  if ("lmerModLmerTest" %in% class(fitted_model)) {
    data <- model.frame(fitted_model)
  } else {
    data <- fitted_model$frame
  }
  terms <- attr(terms(original_formula), "term.labels")
  terms <- terms[!grepl("\\|", terms)] # Exclude random effects
  # Function to check if all components of a term appear in other terms
  is_part_of_other_term <- function(term, terms) {
    if (length(terms) == 0) {
      return(FALSE)
    } else {
      if (length(terms) == 1) {
        return(FALSE)
      } else {
        components <- unlist(strsplit(term, split = ":"))
        matches <- sapply(components, function(comp) grepl(paste0("\\b", comp, "\\b"), terms))
        return(apply(matches, 1, all) %>% any)
      }
    }
  }
  terms_update <- terms[!sapply(seq_along(terms), function(i) is_part_of_other_term(terms[i], terms[-i]))]
  anova_results <- list()
  for (term in terms_update) {
    formula_string <- paste0(original_formula_str, " - ", term)
    new_formula <- as.formula(formula_string, env = environment(original_formula))
    new_model <- update(fitted_model, formula = new_formula)
    anova_result <- anova(fitted_model, new_model)
    attr(anova_result, "heading") <- NULL
    anova_results[[term]] <- list(anova = anova_result)
  }
  
  coefficients <- summary(fitted_model)$coefficients
  
  # Significance codes
  significance_codes <- function(p) {
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return(" ")
    }
  }
  
  conditional <- as.data.frame(coefficients$cond)
  conditional$`Estimate` <- format(conditional$`Estimate`, digits = 3, scientific = FALSE)
  conditional$`Std. Error` <- format(conditional$`Std. Error`, digits = 3, scientific = FALSE)
  conditional$`z value` <- format(conditional$`z value`, digits = 3, scientific = FALSE)
  conditional$`Pr(>|z|)` <- format.pval(conditional$`Pr(>|z|)`, digits = 2, eps = 2e-16, scientific = FALSE)
  conditional$Signif <- sapply(conditional$`Pr(>|z|)`, significance_codes)
  
  if (!is.null(coefficients$zi)) {
    ziformula <- as.data.frame(coefficients$zi)
    ziformula$`Estimate` <- format(ziformula$`Estimate`, digits = 3, scientific = FALSE)
    ziformula$`Std. Error` <- format(ziformula$`Std. Error`, digits = 3, scientific = FALSE)
    ziformula$`z value` <- format(ziformula$`z value`, digits = 3, scientific = FALSE)
    ziformula$`Pr(>|z|)` <- format.pval(ziformula$`Pr(>|z|)`, digits = 2, eps = 2e-16, scientific = FALSE)
    ziformula$Signif <- sapply(ziformula$`Pr(>|z|)`, significance_codes)
  } else {
    ziformula <- "No zi_formula"
  }
  
  if (!is.null(coefficients$disp)) {
    dispformula <- as.data.frame(coefficients$disp)
    dispformula$`Estimate` <- format(dispformula$`Estimate`, digits = 3, scientific = FALSE)
    dispformula$`Std. Error` <- format(dispformula$`Std. Error`, digits = 3, scientific = FALSE)
    dispformula$`z value` <- format(dispformula$`z value`, digits = 3, scientific = FALSE)
    dispformula$`Pr(>|z|)` <- format.pval(dispformula$`Pr(>|z|)`, digits = 2, eps = 2e-16, scientific = FALSE)
    dispformula$Signif <- sapply(dispformula$`Pr(>|z|)`, significance_codes)
  } else {
    dispformula <- "No disp_formula"
  }
  
  list(anova = anova_results, summary = conditional, zi_formula = ziformula, disp_formula = dispformula)
}

remove_terms <- function(fitted_model, terms) {
  updated_model <- update(fitted_model, as.formula(paste(". ~ . -", terms)))
  return(updated_model)
}


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
