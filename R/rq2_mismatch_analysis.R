# RQ2: Mismatch analysis between objective predictions and subjective scores
# Project structure:
#   root/code/   : Python scripts
#   root/code/R/ : R scripts
#   root/data/   : input and output data
#   root/data/rq2_outputs : RQ2 output data
# Run from project root:
#   Rscript code/R/rq2_mismatch_analysis.R

# -----------------------------
# 0. Package preparation
# -----------------------------
AUTO_INSTALL <- TRUE
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "writexl",
  "stringr", "purrr", "tibble", "scales", "forcats"
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
  library(forcats)
})

# Publication-style palettes used only for figure aesthetics.
# RQ2 fill scales use unnamed color vectors below to avoid ggplot name-level matching warnings.
mismatch_palette <- c(
  "Objective overestimation" = "#4C78A8",
  "Objective underestimation" = "#E45756"
)
language_palette <- c(
  "Chinese" = "#3C5488",
  "English" = "#E64B35",
  "CN" = "#3C5488",
  "EN" = "#E64B35",
  "中文" = "#3C5488",
  "英文" = "#E64B35"
)

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

output_dir <- file.path(project_root, "data", "rq2_outputs")
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

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

# Full generation-source labels used in figures.
generation_source_levels <- c("Qwen-Image-2512", "Z-Image-Turbo", "SDXL-Turbo")
model_full_label <- function(x) {
  x0 <- stringr::str_trim(as.character(x))
  x_low <- stringr::str_to_lower(x0)
  dplyr::case_when(
    stringr::str_detect(x_low, "qwen") ~ "Qwen-Image-2512",
    stringr::str_detect(x_low, "z[\\s_-]*image|zimage|z-image|z_image") ~ "Z-Image-Turbo",
    stringr::str_detect(x_low, "sdxl") ~ "SDXL-Turbo",
    TRUE ~ x0
  )
}
ordered_generation_source_levels <- function(x) {
  ux <- unique(as.character(x))
  c(generation_source_levels[generation_source_levels %in% ux], setdiff(sort(ux), generation_source_levels))
}
wrap_generation_source_labels <- function(x) {
  as.character(x)
}


# Optional prompt-attribute columns. The script will use only columns that exist in the input file.
optional_condition_candidates <- c(
  "PromptCategory", "PromptMainCategory", "MainCategory", "Category",
  "PromptType", "PromptComplexity", "Complexity",
  "StyleType", "Style", "CulturalContext", "CultureContext", "Culture",
  "TextSymbolRequirement", "TextSymbolRequired", "TextRequirement", "SymbolRequirement"
)

primary_condition_vars <- c("ModelShortName", "Language")

# Mismatch thresholds.
# Residual mismatch: |standardized CV residual| >= 1 SD.
residual_z_threshold <- 1.0
# Strict high-low mismatch: top quartile vs bottom quartile.
high_quartile <- 4
low_quartile <- 1

# Cross-validation settings for residual-based mismatch analysis.
set.seed(20260620)
cv_repeats <- 20
cv_folds <- 5

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

condition_vars <- unique(c(primary_condition_vars, intersect(optional_condition_candidates, names(df_raw))))
condition_vars <- condition_vars[condition_vars %in% names(df_raw)]

analysis_df <- df_raw %>%
  mutate(across(all_of(c(objective_vars, human_vars)), as.numeric)) %>%
  select(any_of(id_vars), any_of(condition_vars), all_of(objective_vars), all_of(human_vars)) %>%
  filter(if_all(all_of(c(objective_vars, human_vars)), ~ !is.na(.x))) %>%
  mutate(.RowID = row_number())

if (nrow(analysis_df) < 30) {
  stop("Too few complete samples for RQ2 analysis: ", nrow(analysis_df))
}

# -----------------------------
# 4. Helper functions
# -----------------------------
safe_sd <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  ifelse(is.na(s) || s == 0, 1, s)
}

zscore_vec <- function(x) {
  s <- safe_sd(x)
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

label_metric <- function(x) {
  dplyr::recode(x, !!!as.list(metric_labels), .default = x)
}

label_human <- function(x) {
  dplyr::recode(x, !!!as.list(human_labels), .default = x)
}

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

mae <- function(obs, pred) {
  mean(abs(obs - pred), na.rm = TRUE)
}

# Repeated k-fold CV predictions for a single human dimension.
# Objective metrics are standardized within each training fold, and the same training means/sds
# are applied to the held-out fold.
cv_lm_predict <- function(data, y_var, x_vars, repeats = 20, folds = 5, seed = 20260620) {
  set.seed(seed)
  n <- nrow(data)
  pred_store <- matrix(NA_real_, nrow = n, ncol = repeats)

  for (r in seq_len(repeats)) {
    fold_id <- sample(rep(seq_len(folds), length.out = n))

    for (f in seq_len(folds)) {
      train_idx <- which(fold_id != f)
      test_idx <- which(fold_id == f)

      train_x <- data[train_idx, x_vars, drop = FALSE]
      test_x <- data[test_idx, x_vars, drop = FALSE]
      y_train <- data[[y_var]][train_idx]

      mu <- vapply(train_x, mean, numeric(1), na.rm = TRUE)
      sdv <- vapply(train_x, safe_sd, numeric(1))

      train_z <- as.data.frame(Map(function(x, m, s) (x - m) / s, train_x, mu, sdv))
      test_z <- as.data.frame(Map(function(x, m, s) (x - m) / s, test_x, mu, sdv))
      names(train_z) <- x_vars
      names(test_z) <- x_vars

      model_df <- train_z %>% mutate(.y = y_train)

      fit <- tryCatch(
        stats::lm(.y ~ ., data = model_df),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        pred_store[test_idx, r] <- mean(y_train, na.rm = TRUE)
      } else {
        pred_store[test_idx, r] <- as.numeric(stats::predict(fit, newdata = test_z))
      }
    }
  }

  rowMeans(pred_store, na.rm = TRUE)
}

# -----------------------------
# 5. Residual-based mismatch analysis using CV predictions
# -----------------------------
cv_residuals <- purrr::map_dfr(human_vars, function(hv) {
  pred <- cv_lm_predict(
    data = analysis_df,
    y_var = hv,
    x_vars = objective_vars,
    repeats = cv_repeats,
    folds = cv_folds,
    seed = 20260620 + match(hv, human_vars)
  )

  observed <- analysis_df[[hv]]
  residual <- observed - pred

  tmp <- analysis_df %>%
    select(.RowID, any_of(id_vars), any_of(condition_vars)) %>%
    mutate(
      HumanDimension = hv,
      HumanDimensionLabel = label_human(hv),
      ObservedHumanScore = observed,
      CVPredictedHumanScore = pred,
      CVResidual = residual,
      HumanScoreZ = zscore_vec(observed),
      CVPredictedScoreZ = zscore_vec(pred),
      CVResidualZ = zscore_vec(residual),
      HumanQuartile = dplyr::ntile(ObservedHumanScore, 4),
      PredictedQuartile = dplyr::ntile(CVPredictedHumanScore, 4),
      HumanRank = rank(-ObservedHumanScore, ties.method = "average"),
      PredictedRank = rank(-CVPredictedHumanScore, ties.method = "average"),
      RankGap_PredictedMinusHuman = PredictedRank - HumanRank,
      ResidualMismatchType = case_when(
        CVResidualZ <= -residual_z_threshold ~ "Objective-overestimated",
        CVResidualZ >= residual_z_threshold ~ "Objective-underestimated",
        TRUE ~ "Moderate/aligned"
      ),
      StrictHighLowMismatchType = case_when(
        HumanQuartile == low_quartile & PredictedQuartile == high_quartile ~ "Objective-high / subjective-low",
        HumanQuartile == high_quartile & PredictedQuartile == low_quartile ~ "Subjective-high / objective-low",
        TRUE ~ "Not strict high-low mismatch"
      )
    )

  tmp
})

residual_summary <- cv_residuals %>%
  group_by(HumanDimension, HumanDimensionLabel) %>%
  summarise(
    N = n(),
    MeanObserved = mean(ObservedHumanScore, na.rm = TRUE),
    MeanPredicted = mean(CVPredictedHumanScore, na.rm = TRUE),
    MeanResidual = mean(CVResidual, na.rm = TRUE),
    SDResidual = sd(CVResidual, na.rm = TRUE),
    RMSE = rmse(ObservedHumanScore, CVPredictedHumanScore),
    MAE = mae(ObservedHumanScore, CVPredictedHumanScore),
    MeanAbsResidualZ = mean(abs(CVResidualZ), na.rm = TRUE),
    ResidualMismatchCount = sum(abs(CVResidualZ) >= residual_z_threshold, na.rm = TRUE),
    ResidualMismatchRate = ResidualMismatchCount / N,
    StrictHighLowMismatchCount = sum(StrictHighLowMismatchType != "Not strict high-low mismatch", na.rm = TRUE),
    StrictHighLowMismatchRate = StrictHighLowMismatchCount / N,
    .groups = "drop"
  )

mismatch_counts <- cv_residuals %>%
  count(HumanDimension, HumanDimensionLabel, ResidualMismatchType, name = "Count") %>%
  group_by(HumanDimension, HumanDimensionLabel) %>%
  mutate(Rate = Count / sum(Count)) %>%
  ungroup()

strict_high_low_counts <- cv_residuals %>%
  count(HumanDimension, HumanDimensionLabel, StrictHighLowMismatchType, name = "Count") %>%
  group_by(HumanDimension, HumanDimensionLabel) %>%
  mutate(Rate = Count / sum(Count)) %>%
  ungroup()

objective_overestimated_cases <- cv_residuals %>%
  filter(ResidualMismatchType == "Objective-overestimated") %>%
  group_by(HumanDimension, HumanDimensionLabel) %>%
  arrange(CVResidualZ, .by_group = TRUE) %>%
  slice_head(n = 15) %>%
  ungroup()

objective_underestimated_cases <- cv_residuals %>%
  filter(ResidualMismatchType == "Objective-underestimated") %>%
  group_by(HumanDimension, HumanDimensionLabel) %>%
  arrange(desc(CVResidualZ), .by_group = TRUE) %>%
  slice_head(n = 15) %>%
  ungroup()

strict_high_low_cases <- cv_residuals %>%
  filter(StrictHighLowMismatchType != "Not strict high-low mismatch") %>%
  arrange(HumanDimension, StrictHighLowMismatchType, desc(abs(RankGap_PredictedMinusHuman)))

rank_gap_cases <- cv_residuals %>%
  group_by(HumanDimension, HumanDimensionLabel) %>%
  arrange(desc(abs(RankGap_PredictedMinusHuman)), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup()

# -----------------------------
# 6. Condition-level mismatch summaries
# -----------------------------
condition_summary <- purrr::map_dfr(condition_vars, function(cv) {
  cv_residuals %>%
    filter(!is.na(.data[[cv]])) %>%
    group_by(HumanDimension, HumanDimensionLabel, ConditionVariable = cv, ConditionValue = as.character(.data[[cv]])) %>%
    summarise(
      N = n(),
      MeanObserved = mean(ObservedHumanScore, na.rm = TRUE),
      MeanPredicted = mean(CVPredictedHumanScore, na.rm = TRUE),
      MeanResidual = mean(CVResidual, na.rm = TRUE),
      MeanAbsResidual = mean(abs(CVResidual), na.rm = TRUE),
      MeanAbsResidualZ = mean(abs(CVResidualZ), na.rm = TRUE),
      ObjectiveOverestimatedCount = sum(ResidualMismatchType == "Objective-overestimated", na.rm = TRUE),
      ObjectiveUnderestimatedCount = sum(ResidualMismatchType == "Objective-underestimated", na.rm = TRUE),
      ResidualMismatchCount = sum(abs(CVResidualZ) >= residual_z_threshold, na.rm = TRUE),
      ResidualMismatchRate = ResidualMismatchCount / N,
      StrictHighLowMismatchCount = sum(StrictHighLowMismatchType != "Not strict high-low mismatch", na.rm = TRUE),
      StrictHighLowMismatchRate = StrictHighLowMismatchCount / N,
      .groups = "drop"
    )
})

model_language_summary <- cv_residuals %>%
  group_by(HumanDimension, HumanDimensionLabel, ModelShortName, Language) %>%
  summarise(
    N = n(),
    MeanObserved = mean(ObservedHumanScore, na.rm = TRUE),
    MeanPredicted = mean(CVPredictedHumanScore, na.rm = TRUE),
    MeanResidual = mean(CVResidual, na.rm = TRUE),
    MeanAbsResidualZ = mean(abs(CVResidualZ), na.rm = TRUE),
    ObjectiveOverestimatedCount = sum(ResidualMismatchType == "Objective-overestimated", na.rm = TRUE),
    ObjectiveUnderestimatedCount = sum(ResidualMismatchType == "Objective-underestimated", na.rm = TRUE),
    ResidualMismatchRate = mean(abs(CVResidualZ) >= residual_z_threshold, na.rm = TRUE),
    StrictHighLowMismatchRate = mean(StrictHighLowMismatchType != "Not strict high-low mismatch", na.rm = TRUE),
    .groups = "drop"
  )

prompt_mismatch_summary <- cv_residuals %>%
  group_by(HumanDimension, HumanDimensionLabel, PromptID) %>%
  summarise(
    N = n(),
    MeanAbsResidualZ = mean(abs(CVResidualZ), na.rm = TRUE),
    ResidualMismatchCount = sum(abs(CVResidualZ) >= residual_z_threshold, na.rm = TRUE),
    ResidualMismatchRate = ResidualMismatchCount / N,
    StrictHighLowMismatchCount = sum(StrictHighLowMismatchType != "Not strict high-low mismatch", na.rm = TRUE),
    StrictHighLowMismatchRate = StrictHighLowMismatchCount / N,
    .groups = "drop"
  ) %>%
  arrange(HumanDimension, desc(MeanAbsResidualZ))

# -----------------------------
# 7. Individual metric high-low mismatch analysis
# -----------------------------
metric_long <- analysis_df %>%
  select(.RowID, any_of(id_vars), any_of(condition_vars), all_of(objective_vars)) %>%
  pivot_longer(
    cols = all_of(objective_vars),
    names_to = "ObjectiveMetric",
    values_to = "ObjectiveScore"
  ) %>%
  group_by(ObjectiveMetric) %>%
  mutate(
    ObjectiveMetricLabel = label_metric(ObjectiveMetric),
    ObjectiveQuartile = dplyr::ntile(ObjectiveScore, 4),
    ObjectiveScoreZ = zscore_vec(ObjectiveScore)
  ) %>%
  ungroup()

human_long <- analysis_df %>%
  select(.RowID, all_of(human_vars)) %>%
  pivot_longer(
    cols = all_of(human_vars),
    names_to = "HumanDimension",
    values_to = "ObservedHumanScore"
  ) %>%
  group_by(HumanDimension) %>%
  mutate(
    HumanDimensionLabel = label_human(HumanDimension),
    HumanQuartile = dplyr::ntile(ObservedHumanScore, 4),
    HumanScoreZ = zscore_vec(ObservedHumanScore)
  ) %>%
  ungroup()

metric_human_high_low_cases <- metric_long %>%
  inner_join(human_long, by = ".RowID", relationship = "many-to-many") %>%
  mutate(
    MetricHumanMismatchType = case_when(
      ObjectiveQuartile == high_quartile & HumanQuartile == low_quartile ~ "Metric-high / subjective-low",
      ObjectiveQuartile == low_quartile & HumanQuartile == high_quartile ~ "Subjective-high / metric-low",
      TRUE ~ "Not strict high-low mismatch"
    ),
    MetricHumanZGap = ObjectiveScoreZ - HumanScoreZ
  ) %>%
  filter(MetricHumanMismatchType != "Not strict high-low mismatch") %>%
  arrange(HumanDimension, ObjectiveMetric, desc(abs(MetricHumanZGap)))

metric_human_high_low_summary <- metric_long %>%
  inner_join(human_long, by = ".RowID", relationship = "many-to-many") %>%
  mutate(
    MetricHumanMismatchType = case_when(
      ObjectiveQuartile == high_quartile & HumanQuartile == low_quartile ~ "Metric-high / subjective-low",
      ObjectiveQuartile == low_quartile & HumanQuartile == high_quartile ~ "Subjective-high / metric-low",
      TRUE ~ "Not strict high-low mismatch"
    )
  ) %>%
  count(ObjectiveMetric, ObjectiveMetricLabel, HumanDimension, HumanDimensionLabel, MetricHumanMismatchType, name = "Count") %>%
  group_by(ObjectiveMetric, ObjectiveMetricLabel, HumanDimension, HumanDimensionLabel) %>%
  mutate(Rate = Count / sum(Count)) %>%
  ungroup()

# -----------------------------
# 8. Human-dimension internal conflict analysis
# -----------------------------
human_conflict_df <- analysis_df %>%
  select(.RowID, any_of(id_vars), any_of(condition_vars), all_of(human_vars)) %>%
  mutate(
    AdherenceQuartile = dplyr::ntile(MeanAdherence, 4),
    ClarityQuartile = dplyr::ntile(MeanClarity, 4),
    AestheticsQuartile = dplyr::ntile(MeanAesthetics, 4),
    CompositeQuartile = dplyr::ntile(CompositeScore, 4),
    HighClarityLowAdherence = ClarityQuartile == high_quartile & AdherenceQuartile == low_quartile,
    HighAestheticsLowAdherence = AestheticsQuartile == high_quartile & AdherenceQuartile == low_quartile,
    HighVisualLowAdherence = (ClarityQuartile == high_quartile | AestheticsQuartile == high_quartile) & AdherenceQuartile == low_quartile,
    HighAestheticsLowClarity = AestheticsQuartile == high_quartile & ClarityQuartile == low_quartile,
    HighClarityLowAesthetics = ClarityQuartile == high_quartile & AestheticsQuartile == low_quartile
  )

internal_conflict_cases <- human_conflict_df %>%
  filter(
    HighClarityLowAdherence | HighAestheticsLowAdherence | HighVisualLowAdherence |
      HighAestheticsLowClarity | HighClarityLowAesthetics
  )

internal_conflict_summary <- tibble(
  ConflictType = c(
    "High clarity / low adherence",
    "High aesthetics / low adherence",
    "High visual quality / low adherence",
    "High aesthetics / low clarity",
    "High clarity / low aesthetics"
  ),
  Count = c(
    sum(human_conflict_df$HighClarityLowAdherence, na.rm = TRUE),
    sum(human_conflict_df$HighAestheticsLowAdherence, na.rm = TRUE),
    sum(human_conflict_df$HighVisualLowAdherence, na.rm = TRUE),
    sum(human_conflict_df$HighAestheticsLowClarity, na.rm = TRUE),
    sum(human_conflict_df$HighClarityLowAesthetics, na.rm = TRUE)
  )
) %>%
  mutate(
    N = nrow(human_conflict_df),
    Rate = Count / N
  )

internal_conflict_by_model_language <- human_conflict_df %>%
  group_by(ModelShortName, Language) %>%
  summarise(
    N = n(),
    HighClarityLowAdherence = sum(HighClarityLowAdherence, na.rm = TRUE),
    HighAestheticsLowAdherence = sum(HighAestheticsLowAdherence, na.rm = TRUE),
    HighVisualLowAdherence = sum(HighVisualLowAdherence, na.rm = TRUE),
    HighAestheticsLowClarity = sum(HighAestheticsLowClarity, na.rm = TRUE),
    HighClarityLowAesthetics = sum(HighClarityLowAesthetics, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 9. Paper-oriented tables
# -----------------------------
paper_table_mismatch_counts <- mismatch_counts %>%
  filter(ResidualMismatchType != "Moderate/aligned") %>%
  select(HumanDimension = HumanDimensionLabel, MismatchType = ResidualMismatchType, Count, Rate) %>%
  mutate(Rate = round(Rate, 3)) %>%
  arrange(HumanDimension, MismatchType)

paper_table_model_language_composite <- model_language_summary %>%
  filter(HumanDimension == "CompositeScore") %>%
  select(
    ModelShortName, Language, N, MeanObserved, MeanPredicted,
    MeanResidual, MeanAbsResidualZ, ResidualMismatchRate, StrictHighLowMismatchRate
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(ModelShortName, Language)

paper_table_internal_conflict <- internal_conflict_summary %>%
  mutate(Rate = round(Rate, 3))

# -----------------------------
# 10. Compact figures for RQ2
# -----------------------------
# Only figures that directly support the current RQ2 Results section are saved.
# Other diagnostic plots are not exported by default to keep the output directory concise.

# Figure A: residual mismatch count by subjective assessment.
p1 <- mismatch_counts %>%
  filter(ResidualMismatchType != "Moderate/aligned") %>%
  mutate(
    HumanDimensionLabel = factor(HumanDimensionLabel, levels = unname(human_labels)),
    ResidualMismatchType = dplyr::case_when(
      ResidualMismatchType %in% c("Objective-overestimated", "Objective overestimation") ~ "Objective overestimation",
      ResidualMismatchType %in% c("Objective-underestimated", "Objective underestimation") ~ "Objective underestimation",
      TRUE ~ as.character(ResidualMismatchType)
    ),
    ResidualMismatchType = factor(
      ResidualMismatchType,
      levels = c("Objective overestimation", "Objective underestimation")
    )
  ) %>%
  ggplot(aes(x = HumanDimensionLabel, y = Count, fill = ResidualMismatchType)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("#4C78A8", "#E45756"), drop = FALSE) +
  labs(
    title = "Residual mismatches by subjective assessment",
    x = "Subjective assessment",
    y = "Number of mismatch cases",
    fill = "Mismatch type"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "#333333", linewidth = 0.25),
    axis.ticks = element_line(color = "#333333", linewidth = 0.25),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8.5),
    axis.text.y = element_text(size = 8.5),
    axis.title = element_text(size = 9.5),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10.5),
    plot.title.position = "plot",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8),
    legend.position = "top"
  )

ggsave(file.path(figure_dir, "rq2_residual_mismatch_counts.png"), p1, width = 6.5, height = 4.2, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "rq2_residual_mismatch_counts.pdf"), p1, width = 6.5, height = 4.2, bg = "white")

# Figure B: composite residual by model and language.
# This is the main figure used in the RQ2 Results section.
# Prepare a dedicated plotting table for Figure B.
# This avoids accidental grouping problems in geom_boxplot and makes the language colors explicit.
p2_data <- cv_residuals %>%
  filter(HumanDimension == "CompositeScore") %>%
  mutate(
    LanguageRaw = as.character(Language),
    LanguageLower = stringr::str_to_lower(stringr::str_trim(LanguageRaw)),
    PlotLanguage = dplyr::case_when(
      stringr::str_detect(LanguageLower, "chinese|中文|^cn$|^zh$|zh-cn") ~ "Chinese",
      stringr::str_detect(LanguageLower, "english|英文|^en$|^eng$") ~ "English",
      TRUE ~ LanguageRaw
    ),
    PlotLanguage = factor(PlotLanguage, levels = c("Chinese", "English")),
    ModelShortName = model_full_label(ModelShortName),
    ModelShortName = factor(ModelShortName, levels = ordered_generation_source_levels(ModelShortName))
  ) %>%
  filter(!is.na(PlotLanguage))

message(
  "RQ2 Figure B rows by language: ",
  paste(
    names(table(p2_data$PlotLanguage)),
    as.integer(table(p2_data$PlotLanguage)),
    sep = "=",
    collapse = "; "
  )
)

p2 <- ggplot(
  p2_data,
  aes(x = ModelShortName, y = CVResidualZ)
) +
  geom_hline(
    yintercept = c(-residual_z_threshold, residual_z_threshold),
    linetype = "dashed",
    color = "#7A7A7A"
  ) +
  geom_boxplot(
    aes(
      group = interaction(ModelShortName, PlotLanguage),
      fill = PlotLanguage
    ),
    color = "#333333",
    alpha = 0.90,
    outlier.alpha = 0.80,
    outlier.size = 1.6,
    width = 0.62,
    linewidth = 0.45,
    position = position_dodge(width = 0.78)
  ) +
  scale_fill_manual(
    values = c("Chinese" = "#4C78A8", "English" = "#E45756"),
    breaks = c("Chinese", "English"),
    drop = FALSE
  ) +
  labs(
    title = "Composite-score residuals by source and language",
    x = "Generation source",
    y = "Standardized residual\n(subjective score minus predicted score)",
    fill = "Prompt language"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "#333333", linewidth = 0.25),
    axis.ticks = element_line(color = "#333333", linewidth = 0.25),
    axis.text.x = element_text(size = 7.8, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 8.5),
    axis.title = element_text(size = 9.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10.5),
    plot.title.position = "plot",
    plot.margin = margin(8, 12, 8, 18),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 8)
  )

ggsave(file.path(figure_dir, "rq2_composite_residual_by_model_language.png"), p2, width = 6.5, height = 4.4, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "rq2_composite_residual_by_model_language.pdf"), p2, width = 6.5, height = 4.4, bg = "white")

# -----------------------------
# 11. Compact output tables
# -----------------------------
data_overview <- tibble(
  Item = c(
    "Input file",
    "Input sheet",
    "Original rows",
    "Complete rows used",
    "Objective metrics",
    "Subjective assessments",
    "Condition variables used",
    "CV repeats",
    "CV folds",
    "Residual mismatch threshold",
    "Strict high-low mismatch rule"
  ),
  Value = c(
    input_file,
    input_sheet,
    nrow(df_raw),
    nrow(analysis_df),
    paste(objective_vars, collapse = ", "),
    paste(human_vars, collapse = ", "),
    ifelse(length(condition_vars) == 0, "None", paste(condition_vars, collapse = ", ")),
    as.character(cv_repeats),
    as.character(cv_folds),
    paste0("|standardized CV residual| >= ", residual_z_threshold),
    "objective/predicted top quartile vs subjective bottom quartile, or vice versa"
  )
)

# A compact image-level residual table is retained because it is the source
# from which all residual-mismatch counts, cases, and model-language summaries
# can be traced. It avoids exporting many separate intermediate CSV files.
output_id_condition_vars <- unique(c(id_vars, condition_vars))

image_level_residuals <- cv_residuals %>%
  select(
    any_of(output_id_condition_vars),
    HumanDimension,
    HumanDimensionLabel,
    ObservedHumanScore,
    CVPredictedHumanScore,
    CVResidual,
    CVResidualZ,
    HumanQuartile,
    PredictedQuartile,
    ResidualMismatchType,
    StrictHighLowMismatchType,
    RankGap_PredictedMinusHuman
  ) %>%
  arrange(HumanDimension, ModelShortName, Language, PromptID, SeedNo)

# Compact case list for optional qualitative inspection.
# This sheet keeps representative overestimation, underestimation, and strict high-low cases
# without exporting multiple separate case files.
representative_mismatch_cases <- bind_rows(
  objective_overestimated_cases %>%
    mutate(CaseGroup = "Residual: objective-overestimated"),
  objective_underestimated_cases %>%
    mutate(CaseGroup = "Residual: objective-underestimated"),
  strict_high_low_cases %>%
    mutate(CaseGroup = "Strict high-low mismatch")
) %>%
  select(
    CaseGroup,
    HumanDimension,
    HumanDimensionLabel,
    any_of(output_id_condition_vars),
    ObservedHumanScore,
    CVPredictedHumanScore,
    CVResidual,
    CVResidualZ,
    HumanQuartile,
    PredictedQuartile,
    ResidualMismatchType,
    StrictHighLowMismatchType,
    RankGap_PredictedMinusHuman
  ) %>%
  arrange(HumanDimension, CaseGroup, desc(abs(CVResidualZ)))

# Keep the composite model-language table used in the Results section,
# and a full model-language summary for supplementary checks if needed.
model_language_all_dimensions <- model_language_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(HumanDimension, ModelShortName, Language)

# Individual metric high-low mismatch summary is retained because the Methods
# explicitly states that single objective metrics were also examined.
individual_metric_high_low_summary <- metric_human_high_low_summary %>%
  filter(MetricHumanMismatchType != "Not strict high-low mismatch") %>%
  arrange(HumanDimension, ObjectiveMetric, MetricHumanMismatchType) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# Prompt-level summary is retained in compact form because the Methods mentions
# PromptID-level distribution checks. It is not intended as a main-text table.
prompt_level_mismatch_summary <- prompt_mismatch_summary %>%
  filter(ResidualMismatchRate > 0 | StrictHighLowMismatchRate > 0) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(HumanDimension, desc(ResidualMismatchRate), desc(MeanAbsResidualZ))

# Optional condition-level summary is kept only for ModelShortName and Language by default.
# This avoids over-fragmented prompt-attribute tables while preserving the RQ2 focus.
condition_summary_compact <- condition_summary %>%
  filter(ConditionVariable %in% c("ModelShortName", "Language")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(HumanDimension, ConditionVariable, ConditionValue)

output_xlsx <- file.path(output_dir, "rq2_mismatch_analysis_results.xlsx")

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

# Clean old RQ2 outputs from earlier script versions.
# The directory structure is unchanged: outputs remain in rq2_outputs/ and figures/.
clean_previous_outputs <- TRUE
if (clean_previous_outputs) {
  unlink(file.path(output_dir, "rq2_*.csv"))
  unlink(file.path(output_dir, "rq2_*.xlsx"))
  unlink(file.path(output_dir, "README_RQ2_outputs.md"))
  unlink(file.path(figure_dir, "rq2_*.png"))
  unlink(file.path(figure_dir, "rq2_*.pdf"))
}

# Re-save the two selected figures after cleanup.
ggsave(file.path(figure_dir, "rq2_residual_mismatch_counts.png"), p1, width = 6.5, height = 4.2, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "rq2_residual_mismatch_counts.pdf"), p1, width = 6.5, height = 4.2, bg = "white")
ggsave(file.path(figure_dir, "rq2_composite_residual_by_model_language.png"), p2, width = 6.5, height = 4.4, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "rq2_composite_residual_by_model_language.pdf"), p2, width = 6.5, height = 4.4, bg = "white")

writexl::write_xlsx(
  purrr::map(list(
    data_overview = data_overview,
    image_level_residuals = image_level_residuals,
    table3_mismatch_counts = paper_table_mismatch_counts,
    strict_high_low_direction = strict_high_low_counts,
    table4_model_language_comp = paper_table_model_language_composite,
    model_language_all_dims = model_language_all_dimensions,
    individual_metric_high_low = individual_metric_high_low_summary,
    internal_conflict_summary = paper_table_internal_conflict,
    representative_cases = representative_mismatch_cases,
    prompt_level_summary = prompt_level_mismatch_summary,
    condition_summary_compact = condition_summary_compact
  ), rename_output_terms),
  path = output_xlsx
)

# -----------------------------
# 12. Write Chinese README
# -----------------------------
readme_file <- file.path(output_dir, "README_RQ2_outputs.md")
readme_text <- c(
  "# RQ2 输出结果说明",
  "",
  "本目录保存 `code/R/rq2_mismatch_analysis.R` 的输出结果，用于回答 RQ2：客观指标预测与主观评分之间的失配主要表现在哪些情形下。",
  "",
  "## 1. 输出精简原则",
  "",
  "本版本不再导出大量中间 CSV 文件，而是将 RQ2 论文分析真正需要的结果集中写入一个 Excel 工作簿，并仅保留与正文或附录直接相关的图。这样可以避免结果目录中出现大量检查性、过程性或过细分组文件。",
  "",
  "## 2. 主要结果文件",
  "",
  "### rq2_mismatch_analysis_results.xlsx",
  "",
  "RQ2 的主结果文件，包含以下工作表：",
  "",
  "- `data_overview`：记录输入文件、样本量、使用的客观指标、主观评分维度、条件变量和阈值设定。",
  "- `image_level_residuals`：图像层面的交叉验证预测与残差结果。该表是残差失配、严格高低分失配、模型语言分组分析和案例追踪的基础。",
  "- `table3_mismatch_counts`：对应正文表 3，按主观评分维度汇总客观高估、客观低估、大残差失配和严格高低分失配数量。",
  "- `strict_high_low_direction`：严格高低分失配的方向统计，用于支撑正文中“极端失配主要表现为客观高分—主观低分，而未观察到主观高分—客观低分反转”的表述。",
  "- `table4_model_language_comp`：对应正文表 4，展示综合评分维度下不同生成来源和提示词语言组合的残差失配分布。",
  "- `model_language_all_dims`：按生成来源和语言汇总所有主观评分维度的残差失配情况，作为正文表 4 的补充检查。",
  "- `individual_metric_high_low`：单个客观指标与主观评分之间的严格高低分失配统计，用于支撑 Methods 中关于单指标失配检查的说明。",
  "- `internal_conflict_summary`：主观评分维度内部冲突统计，用于支撑正文中关于视觉质量、提示词遵循和审美评分整体较一致的结论。",
  "- `representative_cases`：代表性失配案例，包括客观高估、客观低估和严格高低分失配案例，便于后续需要时选择典型样本。",
  "- `prompt_level_summary`：按 PromptID 汇总的失配情况，仅保留出现残差失配或严格高低分失配的提示词，用于补充检查。",
  "- `condition_summary_compact`：按生成来源和语言整理的紧凑条件汇总，用于支撑失配样本分布分析。",
  "",
  "## 3. 图像文件",
  "",
  "图像位于 `figures/` 子目录，并同时输出 PNG 和 PDF 格式：",
  "",
  "- `rq2_residual_mismatch_counts.png/.pdf`：按主观评分维度展示客观高估和客观低估数量，可作为正文表 3 的辅助图或附录图。",
  "- `rq2_composite_residual_by_model_language.png/.pdf`：按生成来源和提示词语言展示综合评分标准化残差分布，对应正文图 2。",
  "",
  "## 4. 未再单独输出的内容",
  "",
  "以下结果不再单独导出为 CSV 或独立图表：完整残差汇总、单独的客观高估案例表、单独的客观低估案例表、单独的严格高低分案例表、完整提示词属性分组表、观察值与预测值散点图、单指标失配柱状图和内部冲突柱状图。这些内容要么已合并到 Excel 工作簿中，要么与当前 RQ2 正文结论关系较弱。",
  "",
  "## 5. 论文使用建议",
  "",
  "正文可使用 `table3_mismatch_counts`、`table4_model_language_comp` 和 `rq2_composite_residual_by_model_language` 图。`strict_high_low_direction` 和 `internal_conflict_summary` 用于支撑正文中的文字结论。`individual_metric_high_low`、`representative_cases`、`prompt_level_summary` 和 `model_language_all_dims` 可用于附录、复核或后续补充分析。",
  ""
)

writeLines(readme_text, readme_file, useBytes = TRUE)

message("RQ2 compact mismatch analysis completed.")
message("Main Excel output saved to: ", output_xlsx)
message("Figures saved to: ", figure_dir)
message("README saved to: ", readme_file)
