# Libraries----

# Loading the libraries and installing them if not in the Rproj

local({
  pkgs <- c("targets", "tidyverse", "janitor", "ggalluvial", "brms","tidybayes", "bayesplot", "performance", "broom.mixed", "emmeans", "glmmTMB", "DHARMa", "ggtext", "flextable", "ggpubr", "vegan", "conflicted")
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

data_cleaning <- function(data, summit_data_tidy, filefjell_species) {
  data %>%
    # We tidy the data
    clean_names() %>%
    relocate(year) %>%
    rename(any_of(c(summit = "top", weather = "vaer"))) %>%
    mutate(summit = str_replace_all(summit, " ", "_"),
           across(any_of("date"), ~ dmy(.x)),
           across(any_of("weather"), ~ str_replace_all(.x, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_"))),
           across(any_of("recorder"), ~ str_replace_all(.x, c(" \\+ " = "_", "\\+" = "_"))),
           across(any_of("species"), ~ str_replace_all(.x, c(" " = "_", "\\." = "")))) %>%
    # We correct the summit data
    left_join(summit_data_tidy, by = "summit") %>%
    select(!height) %>%
    rename(height = correct_height) %>%
    relocate(height:bedrock, .after = summit) %>%
    # We correct some names / change to our preferred system
    mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo",
                               species == "Cer_lan" ~ "Cer_alp_lan",
                               species == "Jun_trif" ~ "Jun_tri",
                               species == "Poa_x_jem" ~ "Poa_jem",
                               species == "Sil_acu" ~ "Sil_aca",
                               TRUE ~ species)) %>%
    left_join(filefjell_species, by = "species") %>%
    # We adjust names to the current nomenclature
    mutate(species = ifelse(!is.na(new_name), new_name, species)) %>%
    relocate(c(specialisation, functional), .after = species) %>%
    select(!new_name) %>%
    # We set the order we are interested in (from highest to lowest)
    mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")),
           specialisation = factor(specialisation, levels = c("alpine", "generalist"))) %>%
    arrange(summit, year, species)
}



# Plotting----

adj_label <- c(richness = "Species<br>richness",
               new = "New<br>species",
               lost = "Lost<br>species",
               altitude = "Uppermost<br>occurrence",
               alpine = "Specialists",
               generalist = "Generalists",
               period1 = "1972–2008/09",
               period2 = "2008/09–2024/25",
               T1 = "Bare rock",
               T27 = "Boulder fields",
               T13 = "Scree",
               T14 = "Ridges",
               T3 = "Alpine heath",
               T22 = "Alpine grassland",
               T7 = "Snowbeds",
               V6 = "Wet snowbeds")

alluvial_palette <- c("Remained" = "#4DAF4A",
                      "Lost" = "#D95F02",
                      "New" = "#1F78B4", # 66C2A5, 1F78B4
                      "Absent" = "#CCCCCC",
                      "0" = "white",
                      "1" = "#4DAF4A")

colour_mapping <-  list(
  period = c("period1" = "#859395", "period2" = "#f58800"),
  specialisation = c("alpine" = "#859395", "generalist" = "#f58800")
)



# Model results----

mod_summary <- function(mod, seed = 811) {
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
    rename(Period = period, Specialisation = specialisation, Estimate = estimate, SE = std.error, CI_lower = any_of(c("conf.low", "lower.HPD")), CI_upper = any_of(c("conf.high", "upper.HPD")), p_value = p.value) %>%
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
    contrast(method = contrast_matrix, adjust = "mvt")
  set.seed(seed) # For full reproducibility
  # Make into a dataframe with the desired output
  contrast_df <- contrast %>%
    tidy(conf.int = TRUE) %>%
    select(!c(term, null.value)) %>%
    mutate(std.error = if (!"std.error" %in% names(.)) NA_real_ else std.error,
           adj.p.value = if (!"adj.p.value" %in% names(.)) NA_real_ else adj.p.value) %>%
    relocate(std.error, .after = estimate) %>%
    rename(Contrast = contrast, Estimate = estimate, SE = std.error, CI_lower = any_of(c("conf.low", "lower.HPD")), CI_upper = any_of(c("conf.high", "upper.HPD")), p_value = adj.p.value) %>%
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
           Specialisation = factor(Specialisation, levels = c("generalist", "alpine"))) |>
    ggplot(aes(x = Estimate, y = Period, colour = Specialisation)) +
    geom_vline(xintercept = 0, colour = "black") +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), width = 0.4, position = position_dodge(width = 0.6)) +
    scale_y_discrete(labels = adj_label) +
    scale_colour_manual("", values = colour_mapping$specialisation, labels = adj_label) +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          text = element_text(size = 14, family = "serif"),
          axis.title.x = element_text(hjust = 0.35, size = 12, margin = margin(t = 5)),
          axis.text.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_markdown(angle = 0, hjust = 0, vjust = 0.5, margin = margin(r = 30)),
          legend.position = "top",
          legend.box.margin = margin(l = 80),
          legend.key.spacing.x = unit(1, "cm"))
}

clean_ft <- function(tab) {
  tab_ft <- tab |>
    flextable() |>
    bg(part = "header", bg = "black") |>
    color(part = "header", color = "white") |>
    bold(part = "header") |>
    bg(part = "body", bg = "white") |>
    color(part = "body", color = "black") |>
    flextable::font(part = "all", fontname = "Times New Roman") |>
    fontsize(part = "header", size = 13) |>
    fontsize(part = "body", size = 12)
  tab_ft
}

# Model fitness----

model_diagnosis <- function(fitted_model, n_sims = 500) {

  # 1 Simulate residuals
  sim_res <- DHARMa::simulateResiduals(fitted_model, n = n_sims, plot = FALSE)

  # 2 Perform the diagnostic tests
  uniformity_test <- DHARMa::testUniformity(sim_res, plot = FALSE)
  quantiles_test <- DHARMa::testQuantiles(sim_res, plot = FALSE) # Note: running plot = FALSE changes the methods used in the test, which may change the results
  dispersion_test <- DHARMa::testDispersion(sim_res, plot = FALSE)
  zero_inflation_test <- DHARMa::testZeroInflation(sim_res, plot = FALSE)
  outliers_test <- DHARMa::testOutliers(sim_res, plot = FALSE)

  results <- list(
    UNIFORMITY = uniformity_test,
    QUANTILES = quantiles_test,
    DISPERSION = dispersion_test,
    ZERO_INFLATION = zero_inflation_test,
    OUTLIERS = outliers_test
  )

  # 3 Quiet plotting helper (evaluate in caller frame, suppress output)
  in_silence <- function(expr) {
    invisible(capture.output(suppressMessages(suppressWarnings(eval.parent(substitute(expr))))))
  }

  # 4 Generate and display the plots
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mfrow = c(2, 2))
  in_silence(DHARMa::testUniformity(sim_res))
  in_silence(DHARMa::testQuantiles(sim_res))
  in_silence(DHARMa::testDispersion(sim_res))
  in_silence(DHARMa::testOutliers(sim_res))

  return(results)
}

model_homoscedasticity <- function(fitted_model, n_sims = 500, effect_measure = c("sd", "iqr"), make_plots = TRUE, seed = 811, notes = TRUE) {

  effect_measure <- match.arg(effect_measure)

  # 1 Simulate residuals
  set.seed(seed)
  sim_res <- DHARMa::simulateResiduals(fitted_model, n = n_sims, plot = FALSE)

  # 2 Parse terms and data
  response_variable <- all.vars(formula(fitted_model))[1]
  explanatory_variables <- attr(terms(fitted_model), "term.labels")
  single_variables <- explanatory_variables[!grepl("[:/|(]", explanatory_variables)]

  # Frame aligned with model; remove rows with missing response
  df <- fitted_model$frame |>
    dplyr::filter(!is.na(.data[[response_variable]]))

  # Function to remove the console output (used when creating the plots)
  in_silence <- function(expr) {
    capture.output(suppressMessages(suppressWarnings(eval.parent(substitute(expr)))))
  }

  # Optional multi‑panel plots
  if (isTRUE(make_plots)) {
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mfrow = c(2, 2))
  }

  out <- setNames(vector("list", length(single_variables)), single_variables)

  # 3 Loop over predictors
  for (var in single_variables) {
    groups <- df[[var]]
    # (A) DHARMa categorical test: within‑group uniformity + Levene across groups
    test_categorical <- DHARMa::testCategorical(sim_res, catPred = groups, plot = isTRUE(make_plots))

    # (B) EFFECT SIZE(S)
    # -- spread summary per level on DHARMa residuals
    r <- sim_res$scaledResiduals[seq_along(groups)]  # aligned after NA filtering
    group_summary <- dplyr::tibble(grp = groups, r = r) |>
      dplyr::group_by(grp) |>
      dplyr::summarise(
        n = dplyr::n(),
        sd = stats::sd(r),
        iqr = stats::IQR(r),
        .groups = "drop"
      )
    spread_vec <- if (effect_measure == "sd") group_summary$sd else group_summary$iqr
    spread_ratio <- max(spread_vec, na.rm = TRUE) /
      max(min(spread_vec, na.rm = TRUE), .Machine$double.eps)

    # -- partial eta^2 from Levene F and dfs
    levene <- test_categorical$homogeneity
    df1 <- levene$Df[1]
    df2 <- levene$Df[2]
    Fv  <- levene$`F value`[1]
    eta2p <- (Fv * df1) / (Fv * df1 + df2)

    # -- within‑group KS D per level (effect size for uniformity test)
    ks_by_level <- lapply(split(r, groups), function(x) {
      if (length(unique(x)) < 2) return(NA_real_)
      suppressWarnings(stats::ks.test(x, "punif")$statistic[[1]])
    })

    levels <- names(unlist(ks_by_level))
    ks_vec <- unlist(ks_by_level)

    # name the per-level p-values by the same order
    uni_p <- stats::setNames(test_categorical$uniformity$p.value, levels)
    uni_padj <- stats::setNames(test_categorical$uniformity$p.value.cor, levels)

    # HOMOGENEITY: turn Levene table into a tibble
    homogeneity_tbl <- levene

    # Effect sizes table
    effect_sizes_tbl <- tibble::tibble(
      Measure = effect_measure,
      Ratio_max_min = spread_ratio,
      Partial_eta2 = eta2p
    )

    # PER-LEVEL: join group summary with per-level uniformity and KS-D
    per_level_tbl <- group_summary |>
      dplyr::mutate(
        levels = as.character(grp),
        uniformity_p = unname(uni_p[levels]),
        uniformity_p_adj = unname(uni_padj[levels]),
        ks_D_by_level = unname(ks_vec[levels])
      ) |>
      dplyr::select(levels, n, uniformity_p, uniformity_p_adj, sd, iqr, ks_D_by_level)

    # (C) Assemble output for this variable
    out[[var]] <- list(
      Homogeneity = homogeneity_tbl,
      `Effect sizes` = effect_sizes_tbl,
      `Per-level` = per_level_tbl
    )
  }

  # 4 Adding notes for interpretation
  if (isTRUE(notes)) {
    notes_text <- c(
      "Spread ratio (max/min): Use descriptively; consider modelling dispersion when Levene p < 0.05 AND ratio >= ~1.5-2.0.",
      "- DHARMa::testCategorical (Levene) — model variances when significant",
      "Partial eta^2 (Levene): Small ~ 0.01; Medium ~ 0.06; Large ~ 0.14 (context, not a strict rule)",
      "- Cohen-style conventions; see `effectsize` docs",
      "KS D per level: D is max distance between residual ECDF and Uniform(0,1); 0 is perfect. Use adjusted p as primary; treat large D as stronger evidence when it aligns with other diagnostics",
      "- DHARMa vignette: interpret tests jointly; Uniform(0,1) target"
    )
    out <- c(out, list(`Rules of thumb` = notes_text))
  }

  return(out)
}

model_temporal_ac <- function(fitted_model, timev, groupv = NULL, n_sims = 500, notes = TRUE, quiet = FALSE) {
  stopifnot(is.character(timev), length(timev) == 1L)
  if (!timev %in% names(fitted_model$frame)) {
    stop(sprintf("Column '%s' not found in fitted_model$frame.", timev))
  }
  if (!is.null(groupv)) {
    stopifnot(is.character(groupv), length(groupv) == 1L)
    if (!groupv %in% names(fitted_model$frame)) {
      stop(sprintf("Column '%s' not found in fitted_model$frame.", groupv))
    }
  }

  # Simulated residuals (portable; no rotation token to avoid version differences)
  res <- DHARMa::simulateResiduals(fitted_model, n = n_sims, plot = FALSE)

  # Single series (no grouping)
  if (is.null(groupv)) {
    time_vec <- fitted_model$frame[[timev]]
    if (anyDuplicated(time_vec)) {
      res_a <- DHARMa::recalculateResiduals(res, group = time_vec)
      test <- DHARMa::testTemporalAutocorrelation(res_a, time = unique(time_vec), plot = !quiet)
    } else {
      test <- DHARMa::testTemporalAutocorrelation(res, time = time_vec, plot = !quiet)
    }
    out <- list(n_series = 1L, p_values = test$p.value,
                combined_p_fisher = test$p.value,
                prop_p_under_0_05 = as.numeric(test$p.value < 0.05))
    if (!quiet) {
      message(sprintf("Temporal AC (single series): p = %.3f", test$p.value))
    }
    return(out)
  }

  # Panel: per-group tests, combined
  time_vec <- fitted_model$frame[[timev]]
  grp_vec <- fitted_model$frame[[groupv]]
  grp_vals <- unique(grp_vec)

  p_vals <- vapply(grp_vals, function(g) {
    sel <- grp_vec == g
    t_g <- time_vec[sel]
    if (length(unique(t_g)) < 2L) return(NA_real_)
    r_g <- DHARMa::recalculateResiduals(res, sel = sel)
    if (anyDuplicated(t_g)) {
      r_ga <- DHARMa::recalculateResiduals(r_g, group = t_g)
      DHARMa::testTemporalAutocorrelation(r_ga, time = unique(t_g), plot = FALSE)$p.value
    } else {
      DHARMa::testTemporalAutocorrelation(r_g, time = t_g, plot = FALSE)$p.value
    }
  }, numeric(1))

  p_use <- p_vals[is.finite(p_vals)]
  k <- length(p_use)

  if (k == 0) {
    out <- list(n_series = 0L, p_values = p_vals,
                combined_p_fisher = NA_real_, prop_p_under_0_05 = NA_real_)
  } else {
    fisher_stat <- -2 * sum(log(p_use))
    fisher_p <- stats::pchisq(fisher_stat, df = 2 * k, lower.tail = FALSE)
    prop_small <- mean(p_use < 0.05)
    out <- list(n_series = k, p_values = p_vals,
                combined_p_fisher = fisher_p, prop_p_under_0_05 = prop_small)
    if (!quiet) {
      msg <- sprintf("Temporal AC across %d series: Fisher p = %.3g; fraction p<0.05 = %.2f",
                     k, fisher_p, prop_small)
      message(msg)
    }
  }

  if (isTRUE(notes)) {
    alpha <- 0.05
    m <- sum(is.finite(p_vals))
    ub_count <- stats::qbinom(0.95, size = m, prob = alpha)   # 95% null upper bound (count)
    ub_prop <- ub_count / m  # ...as a proportion

    notes_text <- c(
      "Global signal (Fisher p): If Fisher's combined p >= 0.05, treat as no overall temporal AC; investigate if < 0.05.",
      "- Combine per-series p-values with Fisher to get one omnibus test.",
      sprintf("Excess small p-values: Under H0, per-series p-values are ~Uniform(0,1). With m = %d and alpha = %.02f, expect ~m*alpha and a 95%% null upper bound of about %.02f (k <= %d). Start investigating if your prop p<%.02f > %.02f or if a one-sided binomial test vs Binomial(m, alpha) is significant.",
              m, alpha, ub_prop, ub_count, alpha, ub_prop),
      "- Counting p<alpha is binomial under H0; use this as a quick prevalence screen.",
      "FDR screen (BH): If any series remains significant after BH at q <= 0.05, follow up.",
      "- Use stats::p.adjust(p_vals, method = 'BH') for an FDR check.",
      "Borderline results: If key p-values are ~0.03–0.07, re-run diagnostics with n_sims ~ 2000 to stabilise.",
      "- Larger DHARMa simulation counts reduce Monte-Carlo jitter.",
      "Per-series test: DHARMa uses a Durbin–Watson test on uniform residuals; ensure unique time within each series (aggregate within-series if needed).",
      "- See DHARMa::testTemporalAutocorrelation help for details."
    )
    out <- c(out, list(`Rules of thumb` = notes_text))
  }

  return(out)
}

