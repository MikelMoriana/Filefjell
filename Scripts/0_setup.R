# Libraries----

# Loading the libraries and installing them if not in the Rproj

local({
  pkgs <- c("targets", "tidyverse", "janitor", "brms","tidybayes", "bayesplot", "performance", "broom.mixed", "emmeans", "glmmTMB", "DHARMa", "ggtext", "flextable", "ggpubr", "vegan", "conflicted")
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
  brms::dstudent_t,
  brms::pstudent_t,
  brms::qstudent_t,
  brms::rstudent_t,
  brms::rhat,
  stats::chisq.test,
  stats::fisher.test,
  flextable::border,
  flextable::compose,
  flextable::font,
  flextable::rotate
)

options(scipen = 999)



# Data cleaning----

data_tidying <- function(data) {
  data %>%
    clean_names() %>%
    relocate(year) %>% 
    rename(any_of(c(summit = "top", weather = "vaer"))) %>%
    mutate(summit = str_replace_all(summit, " ", "_"),
           across(any_of("date"), ~ dmy(.x)),
           across(any_of("weather"), ~ str_replace_all(.x, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_"))),
           across(any_of("recorder"), ~ str_replace_all(.x, c(" \\+ " = "_", "\\+" = "_"))),
           across(any_of("species"), ~ str_replace_all(.x, c(" " = "_", "\\." = "")))) %>%
    mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Rjupeskareggen", "Krekanosi", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")))
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

adj_label <- c(richness = "Species<br>richness", 
               new = "New<br>species", 
               lost = "Lost<br>species", 
               altitude = "Uppermost<br>occurrence",
               alpine = "Specialist",
               generalist = "Generalist",
               period1 = "1972–2008/09",
               period2 = "2008/09–2024/25",
               T1 = "Bare rock",
               T3 = "Mountain heath,\nleeside and tundra",
               T7 = "Snowbed",
               T14 = "Ridge",
               T22 = "Mountain grassland\nand grass tundra",
               T27 = "Boulderfield")

colour_mapping <-  list(
  period = c("period1" = "#859395", "period2" = "#f58800"),
  specialisation = c("alpine" = "#859395", "generalist" = "#f58800")
)


# Model results----

mod_summary <- function(mod) {
  format_ft <- function(tbl, id_col) {
    tbl %>%
      flextable %>%
      bg(part = "header", bg = "black") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header") %>%
      bg(part = "body", bg = "white") %>%
      color(part = "body", color = "black") %>%
      bold(i = ~ ((CI_lower * CI_upper) > 0)) %>%
      autofit()
  }
  contrast_matrix <- list(
    "1A-2A" = c(-1, 1, 0, 0),
    "1G-2G" = c(0, 0, -1, 1),
    "1A-1G" = c(-1, 0, 1, 0),
    "2A-2G" = c(0, -1, 0, 1)
  )
  
  ## Model
  # Extract the fixed effects of the model and arrange as dataframe
  model_df <- tidy(mod, effects = "fixed", conf.int = TRUE) %>%
    select(!c(effect, component)) %>%
    filter(!grepl("sigma", term)) %>%
    mutate(statistic = if (!"statistic" %in% names(.)) NA_real_ else statistic,
           p.value = if (!"p.value" %in% names(.)) NA_real_ else p.value) %>%
    relocate(c(statistic, p.value), .after = std.error) %>%
    rename(Term = term, Estimate = estimate, SE = std.error, Statistic = statistic, p_value = p.value, CI_lower = conf.low, CI_upper = conf.high) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  # Flextable
  model_ft <- model_df %>%
    format_ft() %>%
    align(part = "all", j = -1, align = "center") %>%
    hline(i = c(1, 3))

  ## Emmeans
  emmeans <- emmeans(mod, ~ period * specialisation)
  # Arrange as dataframe
  emmeans_df <- emmeans %>%
    tidy(conf.int = TRUE) %>%
    mutate(std.error = if (!"std.error" %in% names(.)) NA_real_ else std.error,
           p.value = if (!"p.value" %in% names(.)) NA_real_ else p.value) %>%
    relocate(std.error, .after = estimate) %>%
    rename(Period = period, specialisation = specialisation, Estimate = estimate, SE = std.error, CI_lower = any_of(c("conf.low", "lower.HPD")), CI_upper = any_of(c("conf.high", "upper.HPD")), p_value = p.value) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  # Flextable
  emmeans_ft <-  emmeans_df %>%
    format_ft() %>%
    align(part = "all", j = 3:7, align = "center") %>%
    hline(i = 2) %>%
    vline(j = 2)

  ## Contrasts
  ref_grid <- emmeans |> summary()
  contrast_numbers <- unique(ref_grid[c("period", "specialisation")])
  # Perform contrast analysis with only the desired contrast
  contrast <- emmeans %>%
    contrast(method = contrast_matrix)
  # Make into a dataframe with the desired output
  contrast_df <- contrast %>%
    tidy(conf.int = TRUE) %>%
    select(!c(term, null.value)) %>%
    mutate(std.error = if (!"std.error" %in% names(.)) NA_real_ else std.error,
           p.value = if (!"p.value" %in% names(.)) NA_real_ else p.value) %>%
    relocate(std.error, .after = estimate) %>%
    rename(Contrast = contrast, Estimate = estimate, SE = std.error, CI_lower = any_of(c("conf.low", "lower.HPD")), CI_upper = any_of(c("conf.high", "upper.HPD")), p_value = p.value) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  # Flextable
  contrast_ft <-  contrast_df %>% 
    format_ft() %>%
    align(part = "all", j = 2:6, align = "center") %>%
    hline(i = 2) %>%
    vline(j = 1)
  
  return(list(
    model_ft = model_ft, 
    emmeans = emmeans,
    emmeans_df = emmeans_df,
    emmeans_ft = emmeans_ft,
    contrast_numbers,
    contrast_matrix,
    contrast = contrast,
    contrast_df = contrast_df,
    contrast_ft = contrast_ft))
}

gg_results <- function(data) {
  data |> 
    mutate(Period = factor(Period, levels = c("period2", "period1")),
           specialisation = factor(specialisation, levels = c("generalist", "alpine"))) |>
    ggplot(aes(x = Estimate, y = specialisation, colour = Period)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA)) +
    geom_vline(xintercept = 0, colour = "black") +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.4, position = position_dodge(width = 0.6)) +
    scale_y_discrete(labels = adj_label) +
    scale_colour_manual("Period", values = colour_mapping$period, labels = adj_label) +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme(text = element_text(size = 14, family = "serif"),
          axis.title.x = element_text(hjust = 0.35),
          axis.text.x = element_text(margin = margin(t = 10, b = 10)),
          axis.title.y = element_markdown(angle = 0, hjust = 0, vjust = 0.5, margin = margin(r = 30)),
          panel.grid.major.y = element_blank(),
          legend.position = "top",
          legend.box.margin = margin(l = -10),
          legend.title = element_text(margin = margin(b = 5, r = 40)),
          legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)))
}

mod_types <- function(mod) {
  std_area <- 1
  format_ft <- function(tbl, id_col) {
    tbl %>%
      flextable %>%
      bg(part = "header", bg = "black") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header") %>%
      bg(part = "body", bg = "white") %>%
      color(part = "body", color = "black") %>%
      autofit()
  }
  
  ## Model
  # Extract the fixed effects of the model and arrange as dataframe
  model_df <- mod %>%
    tidy(effects = "fixed", conf.int = TRUE) %>%
    select(!c(effect, component)) %>%
    rename(Term = term, Estimate = estimate, SE = std.error, Statistic = statistic, p_value = p.value, CI_lower = conf.low, CI_upper = conf.high) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  # Flextable
  model_ft <- model_df %>%
    format_ft() %>%
    bold(i = ~ ((CI_lower * CI_upper) > 0)) %>%
    align(part = "all", j = -1, align = "center") %>%
    hline(i = c(1, 9))
  
  ## Emmeans
  reference <- ref_grid(mod,
                        at = list(habitat_decare = std_area))
  emmeans <- reference |>
    emmeans(~ main_type * specialisation, type = "response")
  # Arrange as dataframe
  emmeans_df <- emmeans %>%
    tidy(conf.int = TRUE) %>%
    mutate(main_type = factor(main_type, levels = c("T1", "T27", "T14", "T3", "T22", "T7", "V"))) %>%
    rename(Habitat = main_type, Estimate = response, SE = std.error, CI_lower = conf.low, CI_upper = conf.high, p_value = p.value) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  # Flextable
  emmeans_ft <-  emmeans_df %>%
    format_ft() %>%
    bold(i = ~ p_value < 0.05) %>%
    align(part = "all", j = 2:7, align = "center") %>%
    hline(i = 7) %>%
    vline(j = 1)

  ## Contrasts
  contrast_spe <- emmeans %>%
    contrast(method = "pairwise", by ="specialisation", adjust = "tukey")
  # Make into a dataframe with the desired output
  contrast_spe_df <- contrast_spe %>%
    tidy(conf.int = TRUE) %>%
    select(!c(term, null.value, df, null)) %>%
    rename(Specialisation = specialisation, Contrast = contrast, Ratio = ratio, SE = std.error, Statistic = statistic, CI_lower = conf.low, CI_upper = conf.high, p_value = adj.p.value) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  # Flextable
  contrast_spe_ft <-  contrast_spe_df %>%
    format_ft() %>%
    bold(i = ~ p_value < 0.05) %>%
    align(part = "all", j = 2:6, align = "center") %>%
    hline(i = c(6, 11, 15, 18, 20, 27, 32, 36, 39, 41)) %>%
    hline(i = 21, border = officer::fp_border(style = "thick")) %>%
    vline(j = 1)

  return(list(
    model_ft = model_ft,
    emmeans = emmeans,
    emmeans_df = emmeans_df,
    emmeans_ft = emmeans_ft,
    contrast_spe = contrast_spe,
    contrast_spe_df = contrast_spe_df,
    contrast_spe_ft = contrast_spe_ft))
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
