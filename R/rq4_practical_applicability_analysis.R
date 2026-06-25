# RQ4 computational feasibility and metric-use analysis for objective assessment
# The script integrates subjective alignment, extended prompt-space stability, scoring cost,
# resource usage, optional implementation complexity, and produces a metric-use matrix.

options(stringsAsFactors = FALSE)

required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "ggplot2",
  "writexl", "readr", "scales", "forcats")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

install_if_missing(required_packages)

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  idx <- grep(file_arg, args)
  if (length(idx) > 0) return(normalizePath(sub(file_arg, "", args[idx[1]])))
  if (!is.null(sys.frames()[[1]]$ofile)) return(normalizePath(sys.frames()[[1]]$ofile))
  return(NA_character_)
}

script_path <- get_script_path()
project_root <- if (!is.na(script_path)) {
  normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

data_dir <- file.path(project_root, "data")
rq3_output_dir <- file.path(data_dir, "rq3_outputs")
rq4_output_dir <- file.path(data_dir, "rq4_outputs")
figure_dir <- file.path(rq4_output_dir, "figures")
dir.create(rq4_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

find_input <- function(...) {
  candidates <- c(...)
  candidates <- candidates[!is.na(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Input file not found. Tried: ", paste(candidates, collapse = "; "))
  }
  existing[1]
}

safe_read_excel <- function(path, sheet) {
  readxl::read_excel(path, sheet = sheet, .name_repair = "unique")
}

standardize_metric_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    str_detect(x, regex("OpenCLIP", ignore_case = TRUE)) ~ "OpenCLIP",
    str_detect(x, regex("CNCLIP", ignore_case = TRUE)) ~ "CNCLIP",
    str_detect(x, regex("Aesthetic", ignore_case = TRUE)) ~ "Aesthetic",
    str_detect(x, regex("MUSIQ", ignore_case = TRUE)) ~ "MUSIQ",
    str_detect(x, regex("ImageReward", ignore_case = TRUE)) ~ "ImageReward",
    str_detect(x, regex("HPS", ignore_case = TRUE)) ~ "HPS v2.1",
    str_detect(x, regex("VQAScore", ignore_case = TRUE)) ~ "VQAScore",
    TRUE ~ x
  )
}


standardize_model_name <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    str_detect(x, regex("qwen", ignore_case = TRUE)) ~ "qwen",
    str_detect(x, regex("sdxl", ignore_case = TRUE)) ~ "sdxl",
    str_detect(x, regex("z[ -]?image", ignore_case = TRUE)) ~ "zimage",
    TRUE ~ str_to_lower(str_trim(x))
  )
}

normalize_high <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[1], rng[2]))) return(ifelse(is.na(x), NA_real_, 100))
  100 * (x - rng[1]) / (rng[2] - rng[1])
}

normalize_low <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[1], rng[2]))) return(ifelse(is.na(x), NA_real_, 100))
  100 * (rng[2] - x) / (rng[2] - rng[1])
}

score_tier <- function(x, high_cut = 66.7, moderate_cut = 33.3) {
  dplyr::case_when(
    is.na(x) ~ "Not available",
    x >= high_cut ~ "High",
    x >= moderate_cut ~ "Moderate",
    TRUE ~ "Low"
  )
}

plot_theme_sci <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      axis.text = element_text(color = "#333333")
    )
}

metric_use_tier_palette <- c("High" = "#1B9E77", "Moderate" = "#4C78A8", "Low" = "#D95F02")
score_heatmap_palette <- c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")
resource_palette <- c("Median seconds per image" = "#4C78A8", "Max GPU memory (GB)" = "#E45756")

# -----------------------------------------------------------------------------
# Input files
# -----------------------------------------------------------------------------
rq1_file <- find_input(
  file.path(data_dir, "rq1_subjective_objective_consistency_results.xlsx"),
  file.path(data_dir, "rq1_outputs", "rq1_subjective_objective_consistency_results.xlsx")
)
rq3_file <- find_input(
  file.path(rq3_output_dir, "rq3_key_results.xlsx"),
  file.path(data_dir, "rq3_key_results.xlsx")
)
core_resource_file <- find_input(file.path(data_dir, "objective_scores_core360_resources.xlsx"))
extended_resource_file <- find_input(file.path(data_dir, "objective_scores_extended1440_resources.xlsx"))

complexity_csv <- file.path(data_dir, "metric_deployment_complexity.csv")
complexity_xlsx <- file.path(data_dir, "metric_deployment_complexity.xlsx")

# -----------------------------------------------------------------------------
# RQ1: subjective-objective alignment evidence
# -----------------------------------------------------------------------------
spearman_table <- safe_read_excel(rq1_file, "paper_table_spearman")
metric_col <- names(spearman_table)[1]

human_alignment_long <- spearman_table %>%
  rename(MetricLabel = all_of(metric_col)) %>%
  mutate(MetricLabel = standardize_metric_label(MetricLabel)) %>%
  pivot_longer(-MetricLabel, names_to = "HumanDimension", values_to = "SpearmanText") %>%
  mutate(
    Spearman = readr::parse_number(as.character(SpearmanText)),
    IsSignificant = str_detect(as.character(SpearmanText), "\\*")
  )

human_alignment <- human_alignment_long %>%
  group_by(MetricLabel) %>%
  summarise(
    MeanSpearman = mean(Spearman, na.rm = TRUE),
    CompositeSpearman = Spearman[HumanDimension == "Composite score"][1],
    MaxSpearman = max(Spearman, na.rm = TRUE),
    SignificantDimensionCount = sum(IsSignificant, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    HumanAlignmentScore = ifelse(MaxSpearman <= 0, 0, 100 * pmax(MeanSpearman, 0) / max(pmax(MeanSpearman, 0), na.rm = TRUE)),
    HumanAlignmentTier = case_when(
      MeanSpearman >= 0.35 ~ "High",
      MeanSpearman >= 0.20 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

combined_prediction <- safe_read_excel(rq1_file, "paper_table_combined_prediction")

# -----------------------------------------------------------------------------
# RQ3: extended prompt-space stability evidence
# -----------------------------------------------------------------------------
correlation_stability <- safe_read_excel(rq3_file, "correlation_stability")
model_rank_stability <- safe_read_excel(rq3_file, "model_rank_stability") %>%
  mutate(MetricLabel = standardize_metric_label(MetricLabel))
normalize_dataset_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    str_detect(x, regex("core", ignore_case = TRUE)) ~ "Core360",
    str_detect(x, regex("extended", ignore_case = TRUE)) ~ "Extended1440",
    TRUE ~ x
  )
}

language_sensitivity <- safe_read_excel(rq3_file, "language_sensitivity_by_model")
required_language_input_columns <- c("Dataset", "ModelShortName", "MeanShift")
missing_language_input_columns <- setdiff(required_language_input_columns, names(language_sensitivity))
if (length(missing_language_input_columns) > 0) {
  stop(
    "Sheet 'language_sensitivity_by_model' is missing required columns: ",
    paste(missing_language_input_columns, collapse = ", ")
  )
}
if (!"MetricLabel" %in% names(language_sensitivity) && "ObjectiveMetric" %in% names(language_sensitivity)) {
  language_sensitivity$MetricLabel <- language_sensitivity$ObjectiveMetric
}
if (!"MetricLabel" %in% names(language_sensitivity)) {
  stop("Sheet 'language_sensitivity_by_model' must contain either 'MetricLabel' or 'ObjectiveMetric'.")
}
language_sensitivity <- language_sensitivity %>%
  mutate(
    Dataset = normalize_dataset_label(Dataset),
    ModelShortName = standardize_model_name(ModelShortName),
    MetricLabel = standardize_metric_label(MetricLabel)
  )

metric_rank_stability <- model_rank_stability %>%
  mutate(
    RankSpearman_Core_vs_Extended = as.numeric(RankSpearman_Core_vs_Extended),
    MeanRankAbsDiff = as.numeric(MeanRankAbsDiff),
    RankChangedModels = as.numeric(RankChangedModels),
    MeanScoreSpearman_Core_vs_Extended = as.numeric(MeanScoreSpearman_Core_vs_Extended)
  ) %>%
  group_by(MetricLabel) %>%
  summarise(
    MeanRankStability = mean(RankSpearman_Core_vs_Extended, na.rm = TRUE),
    MeanScoreStability = mean(MeanScoreSpearman_Core_vs_Extended, na.rm = TRUE),
    MeanRankAbsDiff = mean(MeanRankAbsDiff, na.rm = TRUE),
    TotalRankChangedModels = sum(RankChangedModels, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    RankStabilityScore = pmax(0, MeanRankStability) * 100,
    RankStabilityTier = score_tier(RankStabilityScore, high_cut = 80, moderate_cut = 50)
  )

language_shift_wide <- language_sensitivity %>%
  mutate(
    MeanShift = as.numeric(MeanShift),
    ModelShortName = standardize_model_name(ModelShortName)
  ) %>%
  select(MetricLabel, ModelShortName, Dataset, MeanShift) %>%
  group_by(MetricLabel, ModelShortName, Dataset) %>%
  summarise(MeanShift = mean(MeanShift, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(MetricLabel, ModelShortName),
    names_from = Dataset,
    values_from = MeanShift
  )

required_language_columns <- c("MetricLabel", "ModelShortName", "Core360", "Extended1440")
missing_language_columns <- setdiff(required_language_columns, names(language_shift_wide))
if (length(missing_language_columns) > 0) {
  stop(
    "RQ3 language sensitivity data cannot be aligned. Missing columns after pivot_wider: ",
    paste(missing_language_columns, collapse = ", "),
    ". Check Dataset values in sheet 'language_sensitivity_by_model'."
  )
}

language_shift_pair_check <- language_shift_wide %>%
  mutate(HasCompletePair = stats::complete.cases(Core360, Extended1440)) %>%
  group_by(MetricLabel) %>%
  summarise(
    CompleteShiftPairCount = sum(HasCompletePair),
    MissingPairRows = sum(!HasCompletePair),
    .groups = "drop"
  )

invalid_language_shift_metrics <- language_shift_pair_check %>%
  filter(CompleteShiftPairCount < 3)

if (nrow(invalid_language_shift_metrics) > 0) {
  stop(
    "RQ3 language sensitivity data have incomplete Core360-Extended1440 pairs for: ",
    paste(invalid_language_shift_metrics$MetricLabel, collapse = ", "),
    ". Each metric should have three complete model-level pairs."
  )
}

language_shift_stability <- language_shift_wide %>%
  group_by(MetricLabel) %>%
  summarise(
    CompleteShiftPairCount = sum(stats::complete.cases(Core360, Extended1440)),
    CoreExtendedShiftSpearman = suppressWarnings(stats::cor(Core360, Extended1440, method = "spearman")),
    MeanAbsCoreExtendedShiftDifference = mean(abs(Extended1440 - Core360)),
    .groups = "drop"
  )

metric_stability <- metric_rank_stability %>%
  left_join(language_shift_stability, by = "MetricLabel") %>%
  mutate(
    LanguageShiftConsistencyScore = normalize_low(MeanAbsCoreExtendedShiftDifference),
    ExtendedStabilityScore = rowMeans(
      cbind(RankStabilityScore, LanguageShiftConsistencyScore),
      na.rm = TRUE
    ),
    ExtendedStabilityTier = score_tier(ExtendedStabilityScore, high_cut = 75, moderate_cut = 50)
  )

# -----------------------------------------------------------------------------
# Resource and scoring-cost evidence
# -----------------------------------------------------------------------------
read_resource_data <- function(path, dataset_label) {
  safe_read_excel(path, "metric_resources") %>%
    mutate(
      Dataset = dataset_label,
      MetricLabel = standardize_metric_label(Metric),
      ElapsedSeconds = as.numeric(ElapsedSeconds),
      ProcessCPUAvgPercent = as.numeric(ProcessCPUAvgPercent),
      ProcessCPUMaxPercent = as.numeric(ProcessCPUMaxPercent),
      ProcessMemoryRSSAvgMB = as.numeric(ProcessMemoryRSSAvgMB),
      ProcessMemoryRSSMaxMB = as.numeric(ProcessMemoryRSSMaxMB),
      SystemCPUAvgPercent = as.numeric(SystemCPUAvgPercent),
      SystemCPUMaxPercent = as.numeric(SystemCPUMaxPercent),
      GPUUtilizationAvgPercent = as.numeric(GPUUtilizationAvgPercent),
      GPUUtilizationMaxPercent = as.numeric(GPUUtilizationMaxPercent),
      GPUMemoryUsedAvgMB = as.numeric(GPUMemoryUsedAvgMB),
      GPUMemoryUsedMaxMB = as.numeric(GPUMemoryUsedMaxMB),
      HasMetricError = !is.na(MetricError) & str_trim(as.character(MetricError)) != "",
      HasMonitorError = !is.na(MonitorError) & str_trim(as.character(MonitorError)) != ""
    )
}

resource_raw <- bind_rows(
  read_resource_data(core_resource_file, "Core360"),
  read_resource_data(extended_resource_file, "Extended1440")
)

valid_resource <- resource_raw %>% filter(!HasMetricError, !is.na(ElapsedSeconds))

resource_by_dataset_metric <- resource_raw %>%
  group_by(Dataset, MetricLabel) %>%
  summarise(
    NTotal = n(),
    NValid = sum(!HasMetricError & !is.na(ElapsedSeconds)),
    MetricErrorCount = sum(HasMetricError, na.rm = TRUE),
    MonitorErrorCount = sum(HasMonitorError, na.rm = TRUE),
    ErrorRate = MetricErrorCount / NTotal,
    TotalElapsedSeconds = sum(ElapsedSeconds[!HasMetricError], na.rm = TRUE),
    MeanSecondsPerImage = mean(ElapsedSeconds[!HasMetricError], na.rm = TRUE),
    MedianSecondsPerImage = median(ElapsedSeconds[!HasMetricError], na.rm = TRUE),
    P90SecondsPerImage = quantile(ElapsedSeconds[!HasMetricError], 0.90, na.rm = TRUE, names = FALSE),
    ImagesPerMinuteMedian = 60 / MedianSecondsPerImage,
    ProcessMemoryRSSMaxMB = max(ProcessMemoryRSSMaxMB[!HasMetricError], na.rm = TRUE),
    GPUMemoryUsedMaxMB = max(GPUMemoryUsedMaxMB[!HasMetricError], na.rm = TRUE),
    GPUUtilizationAvgPercent = mean(GPUUtilizationAvgPercent[!HasMetricError], na.rm = TRUE),
    GPUUtilizationMaxPercent = max(GPUUtilizationMaxPercent[!HasMetricError], na.rm = TRUE),
    ProcessCPUAvgPercent = mean(ProcessCPUAvgPercent[!HasMetricError], na.rm = TRUE),
    ProcessCPUMaxPercent = max(ProcessCPUMaxPercent[!HasMetricError], na.rm = TRUE),
    .groups = "drop"
  )

resource_cost <- resource_raw %>%
  group_by(MetricLabel) %>%
  summarise(
    NTotal = n(),
    NValid = sum(!HasMetricError & !is.na(ElapsedSeconds)),
    MetricErrorCount = sum(HasMetricError, na.rm = TRUE),
    MonitorErrorCount = sum(HasMonitorError, na.rm = TRUE),
    ErrorRate = MetricErrorCount / NTotal,
    TotalElapsedSeconds = sum(ElapsedSeconds[!HasMetricError], na.rm = TRUE),
    MeanSecondsPerImage = mean(ElapsedSeconds[!HasMetricError], na.rm = TRUE),
    MedianSecondsPerImage = median(ElapsedSeconds[!HasMetricError], na.rm = TRUE),
    P90SecondsPerImage = quantile(ElapsedSeconds[!HasMetricError], 0.90, na.rm = TRUE, names = FALSE),
    ImagesPerMinuteMedian = 60 / MedianSecondsPerImage,
    ProcessMemoryRSSMaxMB = max(ProcessMemoryRSSMaxMB[!HasMetricError], na.rm = TRUE),
    GPUMemoryUsedMaxMB = max(GPUMemoryUsedMaxMB[!HasMetricError], na.rm = TRUE),
    GPUUtilizationAvgPercent = mean(GPUUtilizationAvgPercent[!HasMetricError], na.rm = TRUE),
    GPUUtilizationMaxPercent = max(GPUUtilizationMaxPercent[!HasMetricError], na.rm = TRUE),
    ProcessCPUAvgPercent = mean(ProcessCPUAvgPercent[!HasMetricError], na.rm = TRUE),
    ProcessCPUMaxPercent = max(ProcessCPUMaxPercent[!HasMetricError], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SpeedEfficiencyScore = normalize_low(MedianSecondsPerImage),
    ResourceEfficiencyScore = normalize_low(GPUMemoryUsedMaxMB),
    ReliabilityScore = pmax(0, 100 * (1 - ErrorRate)),
    SpeedTier = score_tier(SpeedEfficiencyScore),
    ResourceTier = score_tier(ResourceEfficiencyScore)
  )

# -----------------------------------------------------------------------------
# Optional deployment-complexity evidence
# -----------------------------------------------------------------------------
metric_levels <- c("OpenCLIP", "CNCLIP", "Aesthetic", "MUSIQ", "ImageReward", "HPS v2.1", "VQAScore")
complexity_template <- tibble::tibble(
  MetricLabel = metric_levels,
  EnvironmentIsolation = NA_real_,
  ModelAcquisition = NA_real_,
  RuntimeStability = NA_real_,
  IntegrationEffort = NA_real_,
  Notes = "Fill each numeric dimension from 0 to 3 if deployment-complexity scoring is used."
)

if (file.exists(complexity_csv)) {
  deployment_complexity <- readr::read_csv(complexity_csv, show_col_types = FALSE)
} else if (file.exists(complexity_xlsx)) {
  deployment_complexity <- readxl::read_excel(complexity_xlsx, sheet = 1)
} else {
  deployment_complexity <- complexity_template
  readr::write_csv(complexity_template, file.path(rq4_output_dir, "metric_deployment_complexity_template.csv"))
}

deployment_complexity <- deployment_complexity %>%
  mutate(
    MetricLabel = standardize_metric_label(MetricLabel),
    across(c(EnvironmentIsolation, ModelAcquisition, RuntimeStability, IntegrationEffort), as.numeric),
    DeploymentComplexityScore = EnvironmentIsolation + ModelAcquisition + RuntimeStability + IntegrationEffort,
    DeploymentSimplicityScore = ifelse(
      is.na(DeploymentComplexityScore),
      NA_real_,
      pmax(0, 100 * (1 - DeploymentComplexityScore / 12))
    ),
    DeploymentComplexityTier = case_when(
      is.na(DeploymentComplexityScore) ~ "Not recorded",
      DeploymentComplexityScore <= 3 ~ "Low",
      DeploymentComplexityScore <= 7 ~ "Moderate",
      TRUE ~ "High"
    )
  )

# -----------------------------------------------------------------------------
# Metric-use matrix
# -----------------------------------------------------------------------------
metric_use_score_inputs <- human_alignment %>%
  full_join(metric_stability, by = "MetricLabel") %>%
  full_join(resource_cost, by = "MetricLabel") %>%
  full_join(deployment_complexity, by = "MetricLabel") %>%
  mutate(
    HasDeploymentComplexity = !is.na(DeploymentSimplicityScore)
  )

use_deployment_complexity <- any(metric_use_score_inputs$HasDeploymentComplexity, na.rm = TRUE)

if (use_deployment_complexity) {
  metric_use_matrix_base <- metric_use_score_inputs %>%
    mutate(
      MetricUseScore = 0.50 * HumanAlignmentScore +
        0.20 * ExtendedStabilityScore +
        0.10 * SpeedEfficiencyScore +
        0.05 * ResourceEfficiencyScore +
        0.05 * ReliabilityScore +
        0.10 * DeploymentSimplicityScore
    )
} else {
  metric_use_matrix_base <- metric_use_score_inputs %>%
    mutate(
      MetricUseScore = 0.35 * HumanAlignmentScore +
        0.25 * ExtendedStabilityScore +
        0.20 * SpeedEfficiencyScore +
        0.10 * ResourceEfficiencyScore +
        0.10 * ReliabilityScore
    )
}

metric_use_matrix_base <- metric_use_matrix_base %>%
  mutate(
    MetricUseTier = score_tier(MetricUseScore, high_cut = 70, moderate_cut = 45),
    CostTier = case_when(
      SpeedEfficiencyScore < 33.3 | ResourceEfficiencyScore < 33.3 ~ "High cost",
      SpeedEfficiencyScore < 66.7 | ResourceEfficiencyScore < 66.7 ~ "Moderate cost",
      TRUE ~ "Low cost"
    ),
    SuggestedUse = case_when(
      HumanAlignmentTier == "High" & ExtendedStabilityTier %in% c("High", "Moderate") & CostTier != "High cost" ~
        "Primary auxiliary reference for subjective-aligned evaluation",
      HumanAlignmentTier == "High" & CostTier == "High cost" ~
        "Focused validation metric when subjective alignment is prioritized",
      HumanAlignmentTier == "Moderate" & CostTier != "High cost" ~
        "Secondary reference for screening and diagnostic comparison",
      HumanAlignmentTier == "Low" & CostTier == "Low cost" ~
        "Low-cost diagnostic or descriptive metric",
      TRUE ~ "Fast bilingual semantic reference or screening metric with language-shift caveats"
    )
  ) %>%
  arrange(desc(MetricUseScore))

metric_use_matrix <- metric_use_matrix_base %>%
  select(
    MetricLabel,
    HumanAlignmentTier, MeanSpearman, CompositeSpearman, HumanAlignmentScore,
    ExtendedStabilityTier, MeanRankStability, MeanAbsCoreExtendedShiftDifference, ExtendedStabilityScore,
    MedianSecondsPerImage, ImagesPerMinuteMedian, GPUMemoryUsedMaxMB, ErrorRate,
    SpeedEfficiencyScore, ResourceEfficiencyScore, ReliabilityScore,
    DeploymentComplexityScore, DeploymentComplexityTier, DeploymentSimplicityScore,
    MetricUseScore, MetricUseTier, SuggestedUse
  )

# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------
scatter_plot_data <- metric_use_matrix %>%
  filter(!is.na(MedianSecondsPerImage), !is.na(MeanSpearman)) %>%
  mutate(
    MetricUseTier = factor(MetricUseTier, levels = c("High", "Moderate", "Low")),
    LogSeconds = log10(MedianSecondsPerImage),
    PointSizeNorm = sqrt(GPUMemoryUsedMaxMB / max(GPUMemoryUsedMaxMB, na.rm = TRUE)),
    # All labels are placed on the right side.
    # The offset is computed on the log10 x-axis so that the visual distance is stable.
    LabelX = LogSeconds + 0.055 + 0.030 * PointSizeNorm
  )

x_breaks_raw <- scales::breaks_log(n = 5)(range(scatter_plot_data$MedianSecondsPerImage, na.rm = TRUE))
x_breaks_raw <- x_breaks_raw[x_breaks_raw > 0]
x_breaks_log <- log10(x_breaks_raw)

scatter_plot <- ggplot(
  scatter_plot_data,
  aes(
    x = LogSeconds,
    y = MeanSpearman,
    size = GPUMemoryUsedMaxMB,
    color = MetricUseTier
  )
) +
  geom_point(alpha = 0.85) +
  geom_text(
    data = scatter_plot_data,
    aes(
      x = LabelX,
      y = MeanSpearman,
      label = MetricLabel,
      color = MetricUseTier
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 3.0,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = x_breaks_log,
    labels = scales::label_number()(x_breaks_raw),
    expand = expansion(mult = c(0.08, 0.24))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
  scale_size_continuous(name = "Max GPU memory (MB)", range = c(3, 10)) +
  scale_color_manual(values = metric_use_tier_palette, drop = FALSE, name = "Metric-use class") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Subjective alignment versus scoring cost",
    x = "Median seconds per image (log scale)",
    y = "Mean Spearman correlation with subjective scores"
  ) +
  plot_theme_sci(base_size = 12) +
  theme(
    legend.position = "right",
    plot.margin = margin(8, 36, 8, 8)
  )

ggsave(file.path(figure_dir, "rq4_alignment_cost_scatter.png"), scatter_plot, width = 9.2, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "rq4_alignment_cost_scatter.pdf"), scatter_plot, width = 9.2, height = 5.5, bg = "white")

heatmap_vars <- metric_use_matrix %>%
  transmute(
    MetricLabel,
    `Subjective alignment` = HumanAlignmentScore,
    `Extended stability` = ExtendedStabilityScore,
    `Speed efficiency` = SpeedEfficiencyScore,
    `GPU-memory efficiency` = ResourceEfficiencyScore,
    Reliability = ReliabilityScore,
    `Deployment simplicity` = DeploymentSimplicityScore
  )

if (all(is.na(heatmap_vars$`Deployment simplicity`))) {
  heatmap_vars <- heatmap_vars %>% select(-`Deployment simplicity`)
}

heatmap_data <- heatmap_vars %>%
  pivot_longer(-MetricLabel, names_to = "Dimension", values_to = "Score") %>%
  mutate(
    MetricLabel = forcats::fct_reorder(MetricLabel, Score, .fun = mean, .desc = TRUE, na.rm = TRUE),
    Dimension = factor(Dimension, levels = unique(Dimension))
  )

heatmap_plot <- ggplot(heatmap_data, aes(x = Dimension, y = MetricLabel, fill = Score)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(Score), "NA", round(Score, 0))), size = 3) +
  scale_fill_gradientn(colours = score_heatmap_palette, limits = c(0, 100), na.value = "#E6E6E6") +
  labs(
    title = "Metric-use profile for generated-image assessment",
    x = NULL,
    y = NULL,
    fill = "Score"
  ) +
  plot_theme_sci(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(file.path(figure_dir, "rq4_practical_applicability_heatmap.png"), heatmap_plot, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(figure_dir, "rq4_practical_applicability_heatmap.pdf"), heatmap_plot, width = 9, height = 5.5)

resource_plot_data <- resource_cost %>%
  transmute(
    MetricLabel,
    `Median seconds per image` = MedianSecondsPerImage,
    `Max GPU memory (GB)` = GPUMemoryUsedMaxMB / 1024
  ) %>%
  pivot_longer(-MetricLabel, names_to = "ResourceMeasure", values_to = "Value")

resource_plot <- ggplot(resource_plot_data, aes(x = reorder(MetricLabel, Value), y = Value, fill = ResourceMeasure)) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(~ResourceMeasure, scales = "free_x") +
  scale_fill_manual(values = resource_palette, guide = "none") +
  labs(
    title = "Scoring time and GPU memory use by objective metric",
    x = NULL,
    y = NULL
  ) +
  plot_theme_sci(base_size = 12)

ggsave(file.path(figure_dir, "rq4_resource_cost_summary.png"), resource_plot, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(figure_dir, "rq4_resource_cost_summary.pdf"), resource_plot, width = 9, height = 5.5)

# -----------------------------------------------------------------------------
# Output tables
# -----------------------------------------------------------------------------
rename_output_terms <- function(data) {
  if (is.data.frame(data)) {
    names(data) <- names(data) %>%
      stringr::str_replace_all("Human", "Subjective") %>%
      stringr::str_replace_all("human", "subjective") %>%
      stringr::str_replace_all("Agreement", "Alignment") %>%
      stringr::str_replace_all("agreement", "alignment") %>%
      stringr::str_replace_all("^MetricUseScore$", "PracticalScore") %>%
      stringr::str_replace_all("^MetricUseTier$", "PracticalTier")
  }
  data
}

output_workbook <- list(
  data_overview = tibble::tibble(
    ProjectRoot = project_root,
    RQ1File = rq1_file,
    RQ3File = rq3_file,
    CoreResourceFile = core_resource_file,
    ExtendedResourceFile = extended_resource_file,
    CoreResourceRows = nrow(read_resource_data(core_resource_file, "Core360")),
    ExtendedResourceRows = nrow(read_resource_data(extended_resource_file, "Extended1440")),
    DeploymentComplexityFileUsed = case_when(
      file.exists(complexity_csv) ~ complexity_csv,
      file.exists(complexity_xlsx) ~ complexity_xlsx,
      TRUE ~ "Not provided; template generated in rq4_outputs"
    )
  ),
  metric_subjective_alignment = human_alignment,
  combined_model_reference = combined_prediction,
  inter_metric_stability = correlation_stability,
  metric_stability = metric_stability,
  resource_cost = resource_cost,
  resource_by_dataset_metric = resource_by_dataset_metric,
  deployment_complexity = deployment_complexity,
  practical_matrix = metric_use_matrix,
  figure_data_heatmap = heatmap_data,
  figure_data_scatter = metric_use_matrix %>% select(MetricLabel, MeanSpearman, MedianSecondsPerImage, GPUMemoryUsedMaxMB),
  figure_data_resource = resource_plot_data
)

writexl::write_xlsx(purrr::map(output_workbook, rename_output_terms), file.path(rq4_output_dir, "rq4_practical_applicability_results.xlsx"))
readr::write_csv(rename_output_terms(metric_use_matrix), file.path(rq4_output_dir, "rq4_practical_matrix.csv"))
readr::write_csv(rename_output_terms(resource_cost), file.path(rq4_output_dir, "rq4_metric_resource_cost.csv"))

# -----------------------------------------------------------------------------
# README
# -----------------------------------------------------------------------------
readme_lines <- c(
  "# RQ4 计算可行性与指标使用分析输出说明",
  "",
  "本目录保存 `rq4_practical_applicability_analysis.R` 的输出结果，用于汇总客观指标的主观一致性、扩展稳定性和计算成本。",
  "",
  "## 主要结果工作簿",
  "- `rq4_practical_applicability_results.xlsx`：RQ4 主要结果工作簿。",
  "  - `data_overview`: Input files and row counts used by the script.",
  "  - `metric_subjective_alignment`：来自 RQ1 Spearman 相关的主观—客观一致性证据。",
  "  - `combined_model_reference`：来自 RQ1 的七指标联合预测结果。",
  "  - `inter_metric_stability`：来自 RQ3 的核心—扩展指标间相关结构稳定性。",
  "  - `metric_stability`：基于生成来源排序和语言差异一致性的指标级扩展稳定性。",
  "  - `resource_cost`：按指标汇总的评分耗时、吞吐量、内存、GPU、CPU 和错误记录。",
  "  - `resource_by_dataset_metric`：按提示词集合和指标汇总的资源成本。",
  "  - `deployment_complexity`：可选的实现复杂度评分表；若未提供则输出模板。",
  "  - `practical_matrix`：RQ4 综合指标使用矩阵。",
  "  - `figure_data_*`: Data used to generate the RQ4 figures.",
  "",
  "## CSV outputs",
  "- `rq4_practical_matrix.csv`：便于快速查看的综合指标使用矩阵。",
  "- `rq4_metric_resource_cost.csv`：按指标汇总的评分耗时和资源成本。",
  "- `metric_deployment_complexity_template.csv`：仅在未提供实现复杂度输入文件时生成。填写后重新运行脚本即可纳入综合评分。",
  "",
  "## 图像文件",
  "- `figures/rq4_alignment_cost_scatter.png` and `.pdf`：展示主观一致性与评分耗时之间的关系。横轴为单图中位评分耗时，纵轴为与主观评分的平均 Spearman 相关，点大小表示最大 GPU 显存占用。",
  "- `figures/rq4_practical_applicability_heatmap.png` and `.pdf`：汇总各指标在主观一致性、扩展稳定性、速度效率、GPU 显存效率、可靠性和可选实现简易性方面的表现。",
  "- `figures/rq4_resource_cost_summary.png` and `.pdf`：展示各客观指标的单图中位评分耗时和最大 GPU 显存占用。",
  "",
  "## 说明",
  "- 综合评分用于组织 RQ1、RQ3 和资源监控得到的多维证据，不解释为跨任务或跨环境的通用排名。",
  "- Deployment-complexity scoring is optional. If it is used, each dimension should be scored from 0 to 3 based on the actual local deployment process.",
  "- Resource summaries use median seconds per image to reduce the influence of unusually slow calls."
)
writeLines(readme_lines, file.path(rq4_output_dir, "README.md"))

message("RQ4 analysis complete. Outputs written to: ", rq4_output_dir)
