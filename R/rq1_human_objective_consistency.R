# RQ1: Subjective-objective alignment for generated-image assessment
# Project structure:
#   root/code/   : Python scripts
#   root/code/R/ : R scripts
#   root/data/   : input and output data
# Run from project root:
#   Rscript code/R/rq1_human_objective_consistency.R

# -----------------------------
# 0. Package preparation
# -----------------------------
AUTO_INSTALL <- TRUE
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "writexl",
  "stringr", "purrr", "tibble", "scales", "patchwork"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  if (AUTO_INSTALL) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  } else {
    stop(
      "Missing packages: ", paste(missing_pkgs, collapse = ", "),
      "\nPlease install them before running this script."
    )
  }
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(writexl)
  library(stringr)
  library(purrr)
  library(tibble)
  library(scales)
  library(patchwork)
})

# -----------------------------
# 1. Path configuration
# -----------------------------
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

script_dir <- get_script_dir()
project_root <- dirname(dirname(script_dir))

input_file <- file.path(project_root, "data", "analysis_human_objective_120.xlsx")
input_sheet <- "analysis_120"

output_dir <- file.path(project_root, "data", "rq1_outputs")
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Optional cleanup: remove previous RQ1 outputs from earlier script versions.
# This keeps the output folder focused on the current, manuscript-oriented results.
clean_previous_outputs <- TRUE
if (clean_previous_outputs) {
  old_root_files <- list.files(
    output_dir,
    pattern = "^rq1_.*\\.(csv|xlsx)$|^README_RQ1_outputs\\.md$",
    full.names = TRUE
  )
  old_fig_files <- list.files(
    figure_dir,
    pattern = "^rq1_.*\\.(png|pdf)$",
    full.names = TRUE
  )
  unlink(c(old_root_files, old_fig_files), force = TRUE)
}

# -----------------------------
# 2. Variable settings
# -----------------------------
objective_vars <- c(
  "OpenCLIPScore", "CNCLIPScore", "AestheticScore", "MUSIQScore",
  "ImageReward", "HPSv21", "VQAScore"
)

human_vars <- c(
  "MeanAdherence", "MeanClarity", "MeanAesthetics", "CompositeScore"
)

id_vars <- c(
  "ImageName", "ObjectiveFileName", "ImageKey", "ModelShortName",
  "PromptID", "Language", "Resolution", "Steps", "SeedNo"
)

metric_labels <- c(
  OpenCLIPScore = "OpenCLIP",
  CNCLIPScore = "CNCLIP",
  AestheticScore = "Aesthetic",
  MUSIQScore = "MUSIQ",
  ImageReward = "ImageReward",
  HPSv21 = "HPS v2.1",
  VQAScore = "VQAScore"
)

human_labels <- c(
  MeanAdherence = "Prompt adherence",
  MeanClarity = "Image clarity",
  MeanAesthetics = "Overall aesthetics",
  CompositeScore = "Composite score"
)

# -----------------------------
# 3. Read and validate data
# -----------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

df_raw <- readxl::read_excel(input_file, sheet = input_sheet)

required_cols <- unique(c(id_vars, objective_vars, human_vars))
missing_cols <- setdiff(required_cols, names(df_raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df <- df_raw %>%
  mutate(across(all_of(c(objective_vars, human_vars)), as.numeric))

analysis_df <- df %>%
  select(any_of(id_vars), all_of(objective_vars), all_of(human_vars)) %>%
  filter(if_all(all_of(c(objective_vars, human_vars)), ~ !is.na(.x)))

if (nrow(analysis_df) < 30) {
  warning("The number of complete cases is small: ", nrow(analysis_df))
}

message("Project root: ", project_root)
message("Input rows: ", nrow(df_raw))
message("Complete analysis rows: ", nrow(analysis_df))
message("Output directory: ", output_dir)

# -----------------------------
# 4. Descriptive statistics
# -----------------------------
descriptive_stats <- analysis_df %>%
  select(all_of(c(objective_vars, human_vars))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    VariableLabel = case_when(
      Variable %in% names(metric_labels) ~ unname(metric_labels[Variable]),
      Variable %in% names(human_labels) ~ unname(human_labels[Variable]),
      TRUE ~ Variable
    )
  ) %>%
  select(Variable, VariableLabel, everything())

# -----------------------------
# 5. Correlation analysis
# -----------------------------
significance_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

cor_strength <- function(r) {
  ar <- abs(r)
  case_when(
    is.na(ar) ~ NA_character_,
    ar >= 0.70 ~ "strong",
    ar >= 0.50 ~ "moderate-to-strong",
    ar >= 0.30 ~ "moderate",
    ar >= 0.10 ~ "weak",
    TRUE ~ "very weak"
  )
}

safe_cor_test <- function(x, y, method) {
  idx <- complete.cases(x, y)
  x2 <- x[idx]
  y2 <- y[idx]
  n <- length(x2)

  if (n < 3 || sd(x2) == 0 || sd(y2) == 0) {
    return(tibble(
      N = n,
      Correlation = NA_real_,
      PValue = NA_real_
    ))
  }

  res <- suppressWarnings(cor.test(x2, y2, method = method, exact = FALSE))
  tibble(
    N = n,
    Correlation = unname(res$estimate),
    PValue = res$p.value
  )
}

correlation_long <- tidyr::expand_grid(
  ObjectiveMetric = objective_vars,
  HumanDimension = human_vars,
  Method = c("spearman", "pearson", "kendall")
) %>%
  rowwise() %>%
  mutate(Result = list(safe_cor_test(
    analysis_df[[ObjectiveMetric]],
    analysis_df[[HumanDimension]],
    Method
  ))) %>%
  ungroup() %>%
  unnest(Result) %>%
  group_by(Method) %>%
  mutate(PAdjBH = p.adjust(PValue, method = "BH")) %>%
  ungroup() %>%
  mutate(
    ObjectiveMetricLabel = unname(metric_labels[ObjectiveMetric]),
    HumanDimensionLabel = unname(human_labels[HumanDimension]),
    AbsCorrelation = abs(Correlation),
    Strength = cor_strength(Correlation),
    Stars = significance_stars(PAdjBH),
    Formatted = if_else(
      is.na(Correlation),
      NA_character_,
      paste0(sprintf("%.3f", Correlation), Stars)
    )
  ) %>%
  select(
    Method, ObjectiveMetric, ObjectiveMetricLabel,
    HumanDimension, HumanDimensionLabel,
    N, Correlation, AbsCorrelation, Strength,
    PValue, PAdjBH, Stars, Formatted
  )

make_correlation_matrix <- function(method_name, value_col = "Correlation") {
  correlation_long %>%
    filter(Method == method_name) %>%
    select(ObjectiveMetric, HumanDimension, all_of(value_col)) %>%
    pivot_wider(names_from = HumanDimension, values_from = all_of(value_col)) %>%
    arrange(match(ObjectiveMetric, objective_vars))
}

spearman_matrix <- make_correlation_matrix("spearman", "Correlation")
pearson_matrix <- make_correlation_matrix("pearson", "Correlation")
kendall_matrix <- make_correlation_matrix("kendall", "Correlation")

spearman_main_table <- correlation_long %>%
  filter(Method == "spearman") %>%
  select(ObjectiveMetric, ObjectiveMetricLabel, HumanDimension, Correlation, PValue, PAdjBH, Stars, Strength, Formatted) %>%
  arrange(match(ObjectiveMetric, objective_vars), match(HumanDimension, human_vars))

spearman_main_table_wide <- spearman_main_table %>%
  select(ObjectiveMetric, ObjectiveMetricLabel, HumanDimension, Formatted) %>%
  pivot_wider(names_from = HumanDimension, values_from = Formatted) %>%
  arrange(match(ObjectiveMetric, objective_vars))

# -----------------------------
# 6. Direct ranking consistency: top-K overlap
# -----------------------------
get_image_id <- function(data) {
  if ("ImageKey" %in% names(data)) {
    return(data$ImageKey)
  }
  if ("ImageName" %in% names(data)) {
    return(data$ImageName)
  }
  return(seq_len(nrow(data)))
}

analysis_df$RankImageID <- get_image_id(analysis_df)

topk_overlap_one <- function(metric, human_dim, k) {
  tmp <- analysis_df %>%
    select(RankImageID, all_of(metric), all_of(human_dim)) %>%
    filter(!is.na(.data[[metric]]), !is.na(.data[[human_dim]]))

  if (nrow(tmp) < k) {
    return(tibble(
      ObjectiveMetric = metric,
      HumanDimension = human_dim,
      K = k,
      N = nrow(tmp),
      OverlapCount = NA_integer_,
      OverlapRate = NA_real_,
      Jaccard = NA_real_
    ))
  }

  objective_top <- tmp %>%
    arrange(desc(.data[[metric]]), RankImageID) %>%
    slice_head(n = k) %>%
    pull(RankImageID)

  human_top <- tmp %>%
    arrange(desc(.data[[human_dim]]), RankImageID) %>%
    slice_head(n = k) %>%
    pull(RankImageID)

  overlap_count <- length(intersect(objective_top, human_top))
  tibble(
    ObjectiveMetric = metric,
    HumanDimension = human_dim,
    K = k,
    N = nrow(tmp),
    OverlapCount = overlap_count,
    OverlapRate = overlap_count / k,
    Jaccard = overlap_count / (2 * k - overlap_count)
  )
}

topk_overlap <- tidyr::expand_grid(
  ObjectiveMetric = objective_vars,
  HumanDimension = human_vars,
  K = c(10, 20, 30)
) %>%
  pmap_dfr(~ topk_overlap_one(..1, ..2, ..3)) %>%
  mutate(
    ObjectiveMetricLabel = unname(metric_labels[ObjectiveMetric]),
    HumanDimensionLabel = unname(human_labels[HumanDimension])
  ) %>%
  select(
    ObjectiveMetric, ObjectiveMetricLabel,
    HumanDimension, HumanDimensionLabel,
    K, N, OverlapCount, OverlapRate, Jaccard
  )

# -----------------------------
# 7. Predictive relationship analysis
#    7.1 Single-metric standardized linear models
#    7.2 Combined seven-metric model with repeated 5-fold CV
# -----------------------------
fit_univariate_lm <- function(metric, human_dim) {
  tmp <- analysis_df %>%
    select(x = all_of(metric), y = all_of(human_dim)) %>%
    filter(!is.na(x), !is.na(y))

  if (nrow(tmp) < 5 || sd(tmp$x) == 0 || sd(tmp$y) == 0) {
    return(tibble(
      ObjectiveMetric = metric,
      HumanDimension = human_dim,
      N = nrow(tmp),
      BetaStd = NA_real_,
      Intercept = NA_real_,
      R2 = NA_real_,
      AdjR2 = NA_real_,
      PValue = NA_real_,
      RMSE = NA_real_
    ))
  }

  tmp <- tmp %>% mutate(x_z = as.numeric(scale(x)))
  model <- lm(y ~ x_z, data = tmp)
  sm <- summary(model)
  pred <- predict(model, newdata = tmp)

  tibble(
    ObjectiveMetric = metric,
    HumanDimension = human_dim,
    N = nrow(tmp),
    BetaStd = unname(coef(model)["x_z"]),
    Intercept = unname(coef(model)["(Intercept)"]),
    R2 = sm$r.squared,
    AdjR2 = sm$adj.r.squared,
    PValue = coef(sm)["x_z", "Pr(>|t|)"],
    RMSE = sqrt(mean((tmp$y - pred)^2))
  )
}

univariate_prediction <- tidyr::expand_grid(
  ObjectiveMetric = objective_vars,
  HumanDimension = human_vars
) %>%
  pmap_dfr(~ fit_univariate_lm(..1, ..2)) %>%
  group_by(HumanDimension) %>%
  mutate(PAdjBH = p.adjust(PValue, method = "BH")) %>%
  ungroup() %>%
  mutate(
    ObjectiveMetricLabel = unname(metric_labels[ObjectiveMetric]),
    HumanDimensionLabel = unname(human_labels[HumanDimension]),
    Stars = significance_stars(PAdjBH)
  ) %>%
  select(
    ObjectiveMetric, ObjectiveMetricLabel,
    HumanDimension, HumanDimensionLabel,
    N, BetaStd, R2, AdjR2, PValue, PAdjBH, Stars, RMSE
  ) %>%
  arrange(match(HumanDimension, human_vars), desc(R2))

standardize_train_test <- function(train_x, test_x) {
  mu <- vapply(train_x, mean, numeric(1), na.rm = TRUE)
  sig <- vapply(train_x, sd, numeric(1), na.rm = TRUE)
  sig[is.na(sig) | sig == 0] <- 1

  train_z <- sweep(sweep(train_x, 2, mu, "-"), 2, sig, "/")
  test_z <- sweep(sweep(test_x, 2, mu, "-"), 2, sig, "/")

  list(train = as.data.frame(train_z), test = as.data.frame(test_z))
}

fit_combined_model <- function(human_dim) {
  tmp <- analysis_df %>%
    select(all_of(objective_vars), y = all_of(human_dim)) %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  if (nrow(tmp) < length(objective_vars) + 5 || sd(tmp$y) == 0) {
    return(list(
      summary = tibble(
        HumanDimension = human_dim,
        N = nrow(tmp),
        Model = "combined_7_metrics_lm",
        R2 = NA_real_,
        AdjR2 = NA_real_,
        FStatistic = NA_real_,
        ModelPValue = NA_real_,
        RMSE = NA_real_
      ),
      coefficients = tibble()
    ))
  }

  x_z <- as.data.frame(scale(tmp[, objective_vars]))
  names(x_z) <- objective_vars
  model_df <- bind_cols(y = tmp$y, x_z)
  model <- lm(y ~ ., data = model_df)
  sm <- summary(model)
  pred <- predict(model, newdata = model_df)

  fstat <- sm$fstatistic
  model_p <- if (!is.null(fstat)) {
    pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  } else {
    NA_real_
  }

  coef_table <- as.data.frame(coef(sm)) %>%
    rownames_to_column("Term") %>%
    as_tibble() %>%
    rename(
      Estimate = Estimate,
      StdError = `Std. Error`,
      TStatistic = `t value`,
      PValue = `Pr(>|t|)`
    ) %>%
    filter(Term != "(Intercept)") %>%
    mutate(
      HumanDimension = human_dim,
      HumanDimensionLabel = unname(human_labels[human_dim]),
      ObjectiveMetric = Term,
      ObjectiveMetricLabel = unname(metric_labels[ObjectiveMetric]),
      PAdjBH = p.adjust(PValue, method = "BH"),
      Stars = significance_stars(PAdjBH)
    ) %>%
    select(
      HumanDimension, HumanDimensionLabel,
      ObjectiveMetric, ObjectiveMetricLabel,
      Estimate, StdError, TStatistic, PValue, PAdjBH, Stars
    )

  model_summary <- tibble(
    HumanDimension = human_dim,
    HumanDimensionLabel = unname(human_labels[human_dim]),
    N = nrow(tmp),
    Model = "combined_7_metrics_lm",
    R2 = sm$r.squared,
    AdjR2 = sm$adj.r.squared,
    FStatistic = unname(ifelse(length(fstat) > 0, fstat[1], NA_real_)),
    ModelPValue = unname(model_p),
    RMSE = sqrt(mean((tmp$y - pred)^2))
  )

  list(summary = model_summary, coefficients = coef_table)
}

set.seed(20260620)
cv_combined_model <- function(human_dim, k = 5, repeats = 20) {
  tmp <- analysis_df %>%
    select(all_of(objective_vars), y = all_of(human_dim)) %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  n <- nrow(tmp)
  if (n < k * 2 || sd(tmp$y) == 0) {
    return(tibble(
      HumanDimension = human_dim,
      HumanDimensionLabel = unname(human_labels[human_dim]),
      N = n,
      K = k,
      Repeats = repeats,
      CV_RMSE_Mean = NA_real_,
      CV_RMSE_SD = NA_real_,
      CV_R2_Mean = NA_real_,
      CV_R2_SD = NA_real_
    ))
  }

  cv_results <- vector("list", repeats)

  for (r in seq_len(repeats)) {
    fold_id <- sample(rep(seq_len(k), length.out = n))
    pred_all <- rep(NA_real_, n)

    for (fold in seq_len(k)) {
      train_idx <- fold_id != fold
      test_idx <- fold_id == fold

      train <- tmp[train_idx, , drop = FALSE]
      test <- tmp[test_idx, , drop = FALSE]

      z <- standardize_train_test(
        train_x = train[, objective_vars, drop = FALSE],
        test_x = test[, objective_vars, drop = FALSE]
      )

      train_model_df <- bind_cols(y = train$y, z$train)
      test_model_df <- z$test

      model <- lm(y ~ ., data = train_model_df)
      pred_all[test_idx] <- predict(model, newdata = test_model_df)
    }

    rmse <- sqrt(mean((tmp$y - pred_all)^2, na.rm = TRUE))
    r2 <- 1 - sum((tmp$y - pred_all)^2, na.rm = TRUE) / sum((tmp$y - mean(tmp$y))^2, na.rm = TRUE)

    cv_results[[r]] <- tibble(Repeat = r, RMSE = rmse, R2 = r2)
  }

  bind_rows(cv_results) %>%
    summarise(
      HumanDimension = human_dim,
      HumanDimensionLabel = unname(human_labels[human_dim]),
      N = n,
      K = k,
      Repeats = repeats,
      CV_RMSE_Mean = mean(RMSE, na.rm = TRUE),
      CV_RMSE_SD = sd(RMSE, na.rm = TRUE),
      CV_R2_Mean = mean(R2, na.rm = TRUE),
      CV_R2_SD = sd(R2, na.rm = TRUE),
      .groups = "drop"
    )
}

# Prompt-grouped cross-validation is a robustness check for the combined model.
# In this setting, all images sharing the same PromptID are assigned to the same
# fold. This prevents images generated from the same prompt from appearing in
# both the training and test sets, and therefore evaluates whether the predictive
# association generalizes beyond repeated prompt-level information.
cv_combined_model_grouped <- function(human_dim, group_var = "PromptID", k = 5, repeats = 20) {
  if (!group_var %in% names(analysis_df)) {
    stop("Grouping variable not found for grouped CV: ", group_var)
  }

  tmp <- analysis_df %>%
    select(all_of(group_var), all_of(objective_vars), y = all_of(human_dim)) %>%
    filter(if_all(all_of(c(objective_vars, "y")), ~ !is.na(.x)), !is.na(.data[[group_var]])) %>%
    mutate(GroupID = as.character(.data[[group_var]]))

  n <- nrow(tmp)
  groups <- unique(tmp$GroupID)
  n_groups <- length(groups)

  if (n < k * 2 || n_groups < k || sd(tmp$y) == 0) {
    return(tibble(
      HumanDimension = human_dim,
      HumanDimensionLabel = unname(human_labels[human_dim]),
      N = n,
      GroupVariable = group_var,
      GroupCount = n_groups,
      K = k,
      Repeats = repeats,
      CVType = "prompt_grouped_cv",
      CV_RMSE_Mean = NA_real_,
      CV_RMSE_SD = NA_real_,
      CV_R2_Mean = NA_real_,
      CV_R2_SD = NA_real_
    ))
  }

  cv_results <- vector("list", repeats)

  for (r in seq_len(repeats)) {
    shuffled_groups <- sample(groups, length(groups), replace = FALSE)
    group_fold_df <- tibble(
      GroupID = shuffled_groups,
      FoldID = rep(seq_len(k), length.out = length(shuffled_groups))
    )

    tmp_fold <- tmp %>% left_join(group_fold_df, by = "GroupID")
    pred_all <- rep(NA_real_, n)

    for (fold in seq_len(k)) {
      train_idx <- tmp_fold$FoldID != fold
      test_idx <- tmp_fold$FoldID == fold

      train <- tmp_fold[train_idx, , drop = FALSE]
      test <- tmp_fold[test_idx, , drop = FALSE]

      if (nrow(train) <= length(objective_vars) + 1 || nrow(test) == 0) {
        next
      }

      z <- standardize_train_test(
        train_x = train[, objective_vars, drop = FALSE],
        test_x = test[, objective_vars, drop = FALSE]
      )

      train_model_df <- bind_cols(y = train$y, z$train)
      test_model_df <- z$test

      model <- lm(y ~ ., data = train_model_df)
      pred_all[test_idx] <- predict(model, newdata = test_model_df)
    }

    valid_pred <- !is.na(pred_all)
    if (sum(valid_pred) < 3 || sum((tmp$y[valid_pred] - mean(tmp$y[valid_pred]))^2, na.rm = TRUE) == 0) {
      rmse <- NA_real_
      r2 <- NA_real_
    } else {
      rmse <- sqrt(mean((tmp$y[valid_pred] - pred_all[valid_pred])^2, na.rm = TRUE))
      r2 <- 1 - sum((tmp$y[valid_pred] - pred_all[valid_pred])^2, na.rm = TRUE) /
        sum((tmp$y[valid_pred] - mean(tmp$y[valid_pred]))^2, na.rm = TRUE)
    }

    cv_results[[r]] <- tibble(Repeat = r, RMSE = rmse, R2 = r2)
  }

  bind_rows(cv_results) %>%
    summarise(
      HumanDimension = human_dim,
      HumanDimensionLabel = unname(human_labels[human_dim]),
      N = n,
      GroupVariable = group_var,
      GroupCount = n_groups,
      K = k,
      Repeats = repeats,
      CVType = "prompt_grouped_cv",
      CV_RMSE_Mean = mean(RMSE, na.rm = TRUE),
      CV_RMSE_SD = sd(RMSE, na.rm = TRUE),
      CV_R2_Mean = mean(R2, na.rm = TRUE),
      CV_R2_SD = sd(R2, na.rm = TRUE),
      .groups = "drop"
    )
}

combined_model_list <- lapply(human_vars, fit_combined_model)
combined_prediction_summary <- bind_rows(lapply(combined_model_list, `[[`, "summary"))
combined_prediction_coefficients <- bind_rows(lapply(combined_model_list, `[[`, "coefficients"))
combined_prediction_cv <- bind_rows(lapply(human_vars, cv_combined_model)) %>%
  mutate(CVType = "image_level_repeated_cv")
combined_prediction_prompt_grouped_cv <- bind_rows(lapply(human_vars, cv_combined_model_grouped))
combined_prediction_cv_comparison <- bind_rows(
  combined_prediction_cv %>%
    mutate(
      CVTypeLabel = "Image-level repeated 5-fold CV",
      GroupVariable = NA_character_,
      GroupCount = NA_integer_
    ),
  combined_prediction_prompt_grouped_cv %>%
    mutate(CVTypeLabel = "PromptID-grouped repeated 5-fold CV")
) %>%
  select(
    HumanDimension, HumanDimensionLabel, CVType, CVTypeLabel, N,
    GroupVariable, GroupCount, K, Repeats,
    CV_RMSE_Mean, CV_RMSE_SD, CV_R2_Mean, CV_R2_SD
  ) %>%
  arrange(match(HumanDimension, human_vars), CVType)

# 7.3 VIF diagnostics for the combined seven-metric models
# VIF is used only to diagnose multicollinearity among objective metrics.
# It is not used to judge model fit. If VIF values are high, individual
# regression coefficients should be interpreted cautiously.
calculate_vif <- function(human_dim) {
  tmp <- analysis_df %>%
    select(all_of(objective_vars), y = all_of(human_dim)) %>%
    filter(if_all(everything(), ~ !is.na(.x)))

  if (nrow(tmp) < length(objective_vars) + 5) {
    return(tibble(
      HumanDimension = human_dim,
      HumanDimensionLabel = unname(human_labels[human_dim]),
      ObjectiveMetric = objective_vars,
      ObjectiveMetricLabel = unname(metric_labels[objective_vars]),
      N = nrow(tmp),
      VIF = NA_real_,
      VIFLevel = NA_character_
    ))
  }

  x_z <- as.data.frame(scale(tmp[, objective_vars]))
  names(x_z) <- objective_vars

  vif_values <- vapply(objective_vars, function(metric) {
    others <- setdiff(objective_vars, metric)
    if (length(others) == 0 || sd(x_z[[metric]], na.rm = TRUE) == 0) {
      return(NA_real_)
    }
    aux_df <- x_z[, c(metric, others), drop = FALSE]
    aux_model <- lm(as.formula(paste(metric, "~ .")), data = aux_df)
    r2 <- summary(aux_model)$r.squared
    if (is.na(r2) || r2 >= 1) {
      return(Inf)
    }
    1 / (1 - r2)
  }, numeric(1))

  tibble(
    HumanDimension = human_dim,
    HumanDimensionLabel = unname(human_labels[human_dim]),
    ObjectiveMetric = objective_vars,
    ObjectiveMetricLabel = unname(metric_labels[objective_vars]),
    N = nrow(tmp),
    VIF = unname(vif_values),
    VIFLevel = case_when(
      is.na(VIF) ~ NA_character_,
      is.infinite(VIF) ~ "strong",
      VIF < 5 ~ "acceptable",
      VIF < 10 ~ "moderate",
      TRUE ~ "strong"
    )
  )
}

combined_vif <- bind_rows(lapply(human_vars, calculate_vif))

# -----------------------------
# 8. Identify strongest objective metrics
# -----------------------------
top_composite_metrics <- correlation_long %>%
  filter(Method == "spearman", HumanDimension == "CompositeScore") %>%
  arrange(desc(AbsCorrelation)) %>%
  slice_head(n = 3) %>%
  select(
    ObjectiveMetric, ObjectiveMetricLabel,
    HumanDimension, HumanDimensionLabel,
    N, Correlation, AbsCorrelation, PValue, PAdjBH, Stars, Strength
  )

top_global_pairs <- correlation_long %>%
  filter(Method == "spearman") %>%
  arrange(desc(AbsCorrelation)) %>%
  slice_head(n = 3) %>%
  select(
    ObjectiveMetric, ObjectiveMetricLabel,
    HumanDimension, HumanDimensionLabel,
    N, Correlation, AbsCorrelation, PValue, PAdjBH, Stars, Strength
  )

# -----------------------------
# 9. Figures
# -----------------------------
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.25),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text = element_text(color = "#333333"),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Publication-style palettes used only for figure aesthetics.
metric_point_palette <- c(
  "OpenCLIP" = "#0072B2",
  "CNCLIP" = "#D55E00",
  "Aesthetic" = "#009E73",
  "MUSIQ" = "#CC79A7",
  "ImageReward" = "#56B4E9",
  "HPS v2.1" = "#E69F00",
  "VQAScore" = "#000000"
)
r2_palette <- c(
  "Full-sample R2" = "#4C78A8",
  "Image-level CV R2" = "#F58518",
  "PromptID-grouped CV R2" = "#54A24B"
)

save_plot <- function(plot, filename_stem, width, height) {
  ggsave(
    filename = file.path(figure_dir, paste0(filename_stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  ggsave(
    filename = file.path(figure_dir, paste0(filename_stem, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    bg = "white"
  )
}

rename_output_terms <- function(data) {
  if (is.data.frame(data)) {
    names(data) <- names(data) %>%
      stringr::str_replace_all("Human", "Subjective") %>%
      stringr::str_replace_all("human", "subjective") %>%
      stringr::str_replace_all("Agreement", "Alignment") %>%
      stringr::str_replace_all("agreement", "alignment")
  }
  data
}

# 9.1 Spearman heatmap: the main visual summary of metric-dimension correspondence.
spearman_for_plot <- correlation_long %>%
  filter(Method == "spearman") %>%
  mutate(
    ObjectiveMetricLabel = factor(ObjectiveMetricLabel, levels = unname(metric_labels[objective_vars])),
    HumanDimensionLabel = factor(HumanDimensionLabel, levels = unname(human_labels[human_vars]))
  )

heatmap_plot <- ggplot(spearman_for_plot, aes(x = HumanDimensionLabel, y = ObjectiveMetricLabel, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3.5) +
  scale_fill_gradient2(
    low = "#3B6FB6", mid = "white", high = "#B6403A",
    midpoint = 0, limits = c(-1, 1), name = "Spearman rho"
  ) +
  labs(
    title = "Subjective-objective Spearman alignment",
    x = "Subjective assessment dimension",
    y = "Objective metric"
  ) +
  plot_theme

save_plot(heatmap_plot, "rq1_spearman_correlation_heatmap", width = 9, height = 5.5)

# 9.2 Top-K overlap heatmap: direct ranking consistency between objective and human top-ranked images.
topk_plot_df <- topk_overlap %>%
  mutate(
    ObjectiveMetricLabel = factor(ObjectiveMetricLabel, levels = unname(metric_labels[objective_vars])),
    HumanDimensionLabel = factor(HumanDimensionLabel, levels = unname(human_labels[human_vars])),
    KLabel = factor(paste0("Top ", K), levels = paste0("Top ", c(10, 20, 30))),
    CountLabel = paste0(OverlapCount, "/", K)
  )

topk_heatmap <- ggplot(topk_plot_df, aes(x = HumanDimensionLabel, y = ObjectiveMetricLabel, fill = OverlapRate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = CountLabel), size = 3.2) +
  facet_wrap(~ KLabel, nrow = 1) +
  scale_fill_gradient(low = "white", high = "#3B6FB6", limits = c(0, 1), labels = scales::percent, name = "Overlap rate") +
  labs(
    title = "Top-K overlap between objective-metric and subjective rankings",
    x = "Subjective assessment dimension",
    y = "Objective metric"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(topk_heatmap, "rq1_topk_overlap_heatmap", width = 13, height = 5.5)

# 9.2b Main combined agreement figure for the manuscript body.
# It combines the Spearman heatmap and the Top-K overlap heatmap into a single two-panel figure.
heatmap_panel <- heatmap_plot +
  labs(title = NULL) +
  theme(legend.position = "right")

topk_panel <- topk_heatmap +
  labs(title = NULL) +
  theme(legend.position = "right")

main_agreement_figure <- heatmap_panel + topk_panel +
  plot_layout(widths = c(1.0, 1.45), guides = "collect") +
  plot_annotation(
    title = "Subjective-objective alignment of image assessment metrics",
    tag_levels = "a"
  ) &
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

save_plot(main_agreement_figure, "rq1_main_alignment_figure", width = 18, height = 5.8)

# 9.3 Faceted scatter plot: top 3 metrics against the composite subjective score.
# This figure helps inspect whether the strongest composite-score associations are smooth,
# monotonic, or driven by a few extreme observations.
if (nrow(top_composite_metrics) > 0) {
  top_composite_vars <- top_composite_metrics$ObjectiveMetric

  scatter_composite_df <- analysis_df %>%
    select(all_of(top_composite_vars), CompositeScore) %>%
    pivot_longer(cols = all_of(top_composite_vars), names_to = "ObjectiveMetric", values_to = "MetricValue") %>%
    mutate(
      ObjectiveMetricLabel = factor(
        unname(metric_labels[ObjectiveMetric]),
        levels = unname(metric_labels[top_composite_vars])
      )
    ) %>%
    left_join(
      top_composite_metrics %>%
        transmute(
          ObjectiveMetric,
          LabelText = paste0("rho = ", sprintf("%.3f", Correlation))
        ),
      by = "ObjectiveMetric"
    )

  scatter_composite_plot <- ggplot(scatter_composite_df, aes(x = MetricValue, y = CompositeScore, color = ObjectiveMetricLabel)) +
    geom_point(alpha = 0.78, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "#333333") +
    scale_color_manual(values = metric_point_palette, guide = "none") +
    facet_wrap(~ ObjectiveMetricLabel, scales = "free_x", nrow = 1) +
    geom_text(
      data = scatter_composite_df %>% distinct(ObjectiveMetricLabel, LabelText),
      aes(x = -Inf, y = Inf, label = LabelText),
      inherit.aes = FALSE,
      hjust = -0.1,
      vjust = 1.3,
      size = 3.5
    ) +
    labs(
      title = "Objective metrics associated with subjective composite score",
      x = "Objective metric value",
      y = "subjective composite score"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )

  save_plot(scatter_composite_plot, "rq1_top3_composite_scatter", width = 11, height = 4)
}

# 9.4 Prediction heatmap: single-metric R-squared.
# This figure complements the correlation heatmap by showing single-metric explanatory strength.
prediction_heatmap_df <- univariate_prediction %>%
  mutate(
    ObjectiveMetricLabel = factor(ObjectiveMetricLabel, levels = unname(metric_labels[objective_vars])),
    HumanDimensionLabel = factor(HumanDimensionLabel, levels = unname(human_labels[human_vars]))
  )

prediction_heatmap <- ggplot(prediction_heatmap_df, aes(x = HumanDimensionLabel, y = ObjectiveMetricLabel, fill = R2)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", R2)), size = 3.5) +
  scale_fill_gradient(low = "white", high = "#B6403A", name = "R-squared") +
  labs(
    title = "Single-metric predictive association with subjective scores",
    x = "Subjective assessment dimension",
    y = "Objective metric"
  ) +
  plot_theme

save_plot(prediction_heatmap, "rq1_univariate_prediction_r2_heatmap", width = 9, height = 5.5)

# 9.5 Combined-model R2 comparison: full-sample fit, image-level CV,
# and PromptID-grouped CV robustness check.
combined_r2_plot_df <- bind_rows(
  combined_prediction_summary %>%
    transmute(
      HumanDimension,
      HumanDimensionLabel,
      EstimateType = "Full-sample R2",
      R2 = R2
    ),
  combined_prediction_cv %>%
    transmute(
      HumanDimension,
      HumanDimensionLabel,
      EstimateType = "Image-level CV R2",
      R2 = CV_R2_Mean
    ),
  combined_prediction_prompt_grouped_cv %>%
    transmute(
      HumanDimension,
      HumanDimensionLabel,
      EstimateType = "PromptID-grouped CV R2",
      R2 = CV_R2_Mean
    )
) %>%
  mutate(
    EstimateType = factor(
      EstimateType,
      levels = c("Full-sample R2", "Image-level CV R2", "PromptID-grouped CV R2")
    ),
    HumanDimensionLabel = factor(HumanDimensionLabel, levels = unname(human_labels[human_vars]))
  )

combined_r2_plot <- ggplot(combined_r2_plot_df, aes(x = HumanDimensionLabel, y = R2, fill = EstimateType)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(
    aes(label = sprintf("%.2f", R2)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.1
  ) +
  scale_y_continuous(limits = c(0, max(0.85, max(combined_r2_plot_df$R2, na.rm = TRUE) + 0.08))) +
  scale_fill_manual(values = r2_palette) +
  labs(
    title = "Combined seven-metric prediction of subjective scores",
    subtitle = "PromptID-grouped CV evaluates unseen prompt-level conditions",
    x = "Subjective assessment dimension",
    y = "R-squared",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

save_plot(combined_r2_plot, "rq1_combined_model_r2_comparison", width = 11, height = 5.3)

# -----------------------------
# 10. Paper-ready tables
# -----------------------------
paper_table_spearman <- spearman_main_table %>%
  select(ObjectiveMetricLabel, HumanDimension, Formatted) %>%
  pivot_wider(names_from = HumanDimension, values_from = Formatted) %>%
  arrange(match(ObjectiveMetricLabel, unname(metric_labels[objective_vars]))) %>%
  rename(
    `Objective metric` = ObjectiveMetricLabel,
    `Prompt adherence` = MeanAdherence,
    `Image clarity` = MeanClarity,
    `Overall aesthetics` = MeanAesthetics,
    `Composite score` = CompositeScore
  )

paper_table_combined_prediction <- combined_prediction_summary %>%
  select(HumanDimension, HumanDimensionLabel, N, R2, AdjR2, RMSE) %>%
  left_join(
    combined_prediction_cv %>%
      select(HumanDimension, K, Repeats, CV_R2_Mean, CV_R2_SD, CV_RMSE_Mean, CV_RMSE_SD) %>%
      rename(
        ImageLevel_K = K,
        ImageLevel_Repeats = Repeats,
        ImageLevel_CV_R2_Mean = CV_R2_Mean,
        ImageLevel_CV_R2_SD = CV_R2_SD,
        ImageLevel_CV_RMSE_Mean = CV_RMSE_Mean,
        ImageLevel_CV_RMSE_SD = CV_RMSE_SD
      ),
    by = "HumanDimension"
  ) %>%
  left_join(
    combined_prediction_prompt_grouped_cv %>%
      select(HumanDimension, GroupVariable, GroupCount, K, Repeats, CV_R2_Mean, CV_R2_SD, CV_RMSE_Mean, CV_RMSE_SD) %>%
      rename(
        PromptGrouped_GroupVariable = GroupVariable,
        PromptGrouped_GroupCount = GroupCount,
        PromptGrouped_K = K,
        PromptGrouped_Repeats = Repeats,
        PromptGrouped_CV_R2_Mean = CV_R2_Mean,
        PromptGrouped_CV_R2_SD = CV_R2_SD,
        PromptGrouped_CV_RMSE_Mean = CV_RMSE_Mean,
        PromptGrouped_CV_RMSE_SD = CV_RMSE_SD
      ),
    by = "HumanDimension"
  ) %>%
  mutate(
    `Image-level CV setting` = paste0(ImageLevel_Repeats, " repeats of ", ImageLevel_K, "-fold CV"),
    `Prompt-grouped CV setting` = paste0(
      PromptGrouped_Repeats, " repeats of ", PromptGrouped_K,
      "-fold CV grouped by ", PromptGrouped_GroupVariable,
      " (", PromptGrouped_GroupCount, " groups)"
    ),
    R2 = round(R2, 3),
    AdjR2 = round(AdjR2, 3),
    RMSE = round(RMSE, 3),
    ImageLevel_CV_R2_Mean = round(ImageLevel_CV_R2_Mean, 3),
    ImageLevel_CV_R2_SD = round(ImageLevel_CV_R2_SD, 3),
    ImageLevel_CV_RMSE_Mean = round(ImageLevel_CV_RMSE_Mean, 3),
    ImageLevel_CV_RMSE_SD = round(ImageLevel_CV_RMSE_SD, 3),
    PromptGrouped_CV_R2_Mean = round(PromptGrouped_CV_R2_Mean, 3),
    PromptGrouped_CV_R2_SD = round(PromptGrouped_CV_R2_SD, 3),
    PromptGrouped_CV_RMSE_Mean = round(PromptGrouped_CV_RMSE_Mean, 3),
    PromptGrouped_CV_RMSE_SD = round(PromptGrouped_CV_RMSE_SD, 3)
  ) %>%
  select(
    `Subjective dimension` = HumanDimensionLabel,
    N,
    `Full-sample R2` = R2,
    `Adjusted R2` = AdjR2,
    `Full-sample RMSE` = RMSE,
    `Image-level CV setting`,
    `Image-level CV R2 mean` = ImageLevel_CV_R2_Mean,
    `Image-level CV R2 SD` = ImageLevel_CV_R2_SD,
    `Image-level CV RMSE mean` = ImageLevel_CV_RMSE_Mean,
    `Image-level CV RMSE SD` = ImageLevel_CV_RMSE_SD,
    `Prompt-grouped CV setting`,
    `Prompt-grouped CV R2 mean` = PromptGrouped_CV_R2_Mean,
    `Prompt-grouped CV R2 SD` = PromptGrouped_CV_R2_SD,
    `Prompt-grouped CV RMSE mean` = PromptGrouped_CV_RMSE_Mean,
    `Prompt-grouped CV RMSE SD` = PromptGrouped_CV_RMSE_SD
  )

robust_correlation_supplement <- correlation_long %>%
  filter(Method %in% c("pearson", "kendall")) %>%
  select(
    Method, ObjectiveMetricLabel, HumanDimensionLabel, N,
    Correlation, PValue, PAdjBH, Stars, Strength
  ) %>%
  arrange(Method, match(ObjectiveMetricLabel, unname(metric_labels[objective_vars])), match(HumanDimensionLabel, unname(human_labels[human_vars])))

combined_coefficients_with_vif <- combined_prediction_coefficients %>%
  left_join(
    combined_vif %>% select(HumanDimension, ObjectiveMetric, VIF, VIFLevel),
    by = c("HumanDimension", "ObjectiveMetric")
  ) %>%
  arrange(match(HumanDimension, human_vars), match(ObjectiveMetric, objective_vars))

# -----------------------------
# 11. Export tables
# -----------------------------
output_workbook <- file.path(output_dir, "rq1_subjective_objective_consistency_results.xlsx")

workbook_tables <- list(
    data_overview = tibble(
      ProjectRoot = project_root,
      InputFile = input_file,
      InputSheet = input_sheet,
      InputRows = nrow(df_raw),
      CompleteAnalysisRows = nrow(analysis_df),
      ObjectiveMetricCount = length(objective_vars),
      SubjectiveDimensionCount = length(human_vars),
      UniquePromptIDs = dplyr::n_distinct(analysis_df$PromptID),
      MainCorrelationMethod = "Spearman",
      SupplementaryCorrelationMethods = "Pearson; Kendall",
      TopKValues = "10; 20; 30",
      CombinedModelCV = "20 repeats of image-level 5-fold cross-validation",
      PromptGroupedCombinedModelCV = "20 repeats of 5-fold cross-validation grouped by PromptID"
    ),
    descriptive_stats = descriptive_stats,
    paper_table_spearman = paper_table_spearman,
    spearman_full_results = spearman_main_table,
    robust_correlations = robust_correlation_supplement,
    topk_overlap = topk_overlap,
    univariate_prediction = univariate_prediction,
    paper_table_combined_prediction = paper_table_combined_prediction,
    combined_coefficients_vif = combined_coefficients_with_vif,
    combined_model_cv = combined_prediction_cv,
    prompt_grouped_cv = combined_prediction_prompt_grouped_cv,
    combined_model_cv_comparison = combined_prediction_cv_comparison,
    top_composite_metrics = top_composite_metrics
)

writexl::write_xlsx(
  purrr::map(workbook_tables, rename_output_terms),
  path = output_workbook
)

# -----------------------------
# 12. Write README for output files
# -----------------------------
readme_file <- file.path(output_dir, "README_RQ1_outputs.md")

readme_lines <- c(
  "# RQ1 输出文件说明",
  "",
  "本目录保存 RQ1 脚本的输出结果，用于 RQ1：客观指标与主观评分之间的一致性和维度对应关系分析。",
  "",
  "## 1. 输入数据与样本信息",
  "",
  paste0("- 输入文件：`", input_file, "`"),
  paste0("- 输入 sheet：`", input_sheet, "`"),
  paste0("- 原始输入行数：", nrow(df_raw)),
  paste0("- 完整分析样本数：", nrow(analysis_df)),
  paste0("- 唯一 PromptID 数量：", dplyr::n_distinct(analysis_df$PromptID)),
  "- 分析变量包括 7 个客观指标和 4 个主观评分维度。",
  "- 回归模型中，客观指标在训练数据内进行 z-score 标准化，使不同指标的回归系数具有可比性。",
  "- 新增的 PromptID 分组交叉验证会把同一个 PromptID 下的所有图像放入同一折，避免同一提示词生成的图像同时出现在训练集和测试集中。",
  "",
  "## 2. 主结果工作簿",
  "",
  "### `rq1_subjective_objective_consistency_results.xlsx`",
  "",
  "该工作簿集中保存 RQ1 的主要结果，避免生成过多分散的 CSV 文件。",
  "",
  "- `data_overview`：输入路径、sheet 名称、样本数、指标数、相关性方法、Top-K 设置、普通交叉验证和 PromptID 分组交叉验证设置。",
  "- `descriptive_stats`：客观指标和主观评分维度的描述性统计。",
  "- `paper_table_spearman`：论文正文可直接使用的主观—客观 Spearman 相关性结果表。",
  "- `spearman_full_results`：长表格式的 Spearman 结果，包括原始 p 值、BH 校正 p 值、显著性标记和相关强度标签。",
  "- `robust_correlations`：Pearson 和 Kendall 补充相关性结果。",
  "- `topk_overlap`：Top 10、Top 20 和 Top 30 条件下，客观指标排序与主观评分排序的重叠结果。",
  "- `univariate_prediction`：每个单独客观指标预测各人类评分维度的标准化线性回归结果。",
  "- `paper_table_combined_prediction`：论文正文可用的七指标联合模型结果表，包含全样本拟合、普通图像级交叉验证和 PromptID 分组交叉验证。",
  "- `combined_coefficients_vif`：七指标联合模型的回归系数和 VIF 诊断结果。该表用于补充说明，不用于因果解释。",
  "- `combined_model_cv`：普通图像级重复 20 次 5 折交叉验证结果。该结果用于评估核心图像集合内部的预测关联。",
  "- `prompt_grouped_cv`：新增的 PromptID 分组重复 20 次 5 折交叉验证结果。该结果用于稳健性检查，评估模型对未见提示词的泛化表现。",
  "- `combined_model_cv_comparison`：普通图像级交叉验证与 PromptID 分组交叉验证的长表对比结果，便于绘图和写作。",
  "- `top_composite_metrics`：与总体主观评分 Spearman 相关性绝对值最高的 3 个客观指标。",
  "",
  "## 3. 图像文件",
  "",
  "图像文件保存在 `figures/` 子目录中：",
  "",
  "- `rq1_spearman_correlation_heatmap.png` / `.pdf`：7 个客观指标与 4 个主观评分维度之间的 Spearman 相关性热力图。",
  "- `rq1_topk_overlap_heatmap.png` / `.pdf`：Top-K 排序重叠热力图，展示客观指标与主观高分图像排序的一致性。",
  "- `rq1_main_alignment_figure.png` / `.pdf`：合并主图，将 Spearman 相关热力图与 Top-K 排序重叠热力图并排展示。",
  "- `rq1_top3_composite_scatter.png` / `.pdf`：与总体主观评分最相关的 3 个客观指标散点图，用于观察关联形态和潜在离群点。",
  "- `rq1_univariate_prediction_r2_heatmap.png` / `.pdf`：单指标线性回归 R² 热力图，用于展示不同指标与不同主观评分维度的预测关联强弱。",
  "- `rq1_combined_model_r2_comparison.png` / `.pdf`：七指标联合模型的全样本 R²、普通图像级 CV R² 和 PromptID 分组 CV R² 对比图。新增的 PromptID 分组结果用于稳健性检查。",
  "",
  "## 4. 建议在论文和附录中的使用方式",
  "",
  "- 正文：可使用 `paper_table_spearman` 作为 RQ1 的相关性主表。",
  "- 正文或补充材料：可使用 `paper_table_combined_prediction` 报告七指标联合模型结果，其中 PromptID 分组 CV 可作为稳健性检查列。",
  "- 附录：建议保留 `robust_correlations`、`topk_overlap`、`univariate_prediction`、`combined_coefficients_vif` 和 `combined_model_cv_comparison`。",
  "- 图像：可使用 `rq1_main_alignment_figure` 作为正文主图；若需分开展示，也可分别使用 Spearman 热力图与 Top-K 热力图。`rq1_combined_model_r2_comparison` 可用于展示普通 CV 与 PromptID 分组 CV 的差异。",
  "",
  "## 5. 结果解释提示",
  "",
  "- 普通图像级交叉验证回答的是：在当前主观评分核心图像集合内部，七指标组合能否预测主观评分差异。",
  "- PromptID 分组交叉验证回答的是：当测试图像来自训练中未出现过的提示词时，七指标组合是否仍具有预测关联。",
  "- 如果 PromptID 分组 CV R² 明显低于普通 CV R²，应将原始 CV 结果解释为核心提示词空间内部的预测关联，而不是对未见提示词的强泛化能力。",
  "- 如果 PromptID 分组 CV R² 仍保持为正且接近普通 CV R²，则说明七指标组合的预测关联并非主要由重复提示词信息造成。",
  "",
  "## 6. 显著性标记",
  "",
  "- `*` 表示 BH 校正后 p < 0.05。",
  "- `**` 表示 BH 校正后 p < 0.01。",
  "- `***` 表示 BH 校正后 p < 0.001。",
  "- 图中通常只显示数值系数或数量，以避免视觉元素过于拥挤。"
)

readme_con <- file(readme_file, open = "w", encoding = "UTF-8")
writeLines(readme_lines, readme_con, useBytes = TRUE)
close(readme_con)

# -----------------------------
# 13. Console summary
# -----------------------------
message("\nRQ1 analysis completed.")
message("Main result workbook: ", output_workbook)
message("Figures saved to: ", figure_dir)
message("README saved to: ", readme_file)
message("\nTop 3 objective metrics by Spearman correlation with CompositeScore:")
print(top_composite_metrics)
message("\nCombined seven-metric model summary:")
print(combined_prediction_summary)
message("\nRepeated 5-fold CV summary for combined model:")
print(combined_prediction_cv)
message("\nVIF diagnostics for combined model:")
print(combined_vif)
