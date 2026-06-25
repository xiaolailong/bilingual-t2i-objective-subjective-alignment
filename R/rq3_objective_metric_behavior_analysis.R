############################################################
# RQ3: Objective metric behavior and stability in generated-image assessment
#
# Purpose:
#   Analyze whether objective metrics behave consistently
#   across a larger bilingual prompt space.
#
# Inputs expected under <project_root>/data:
#   - prompts.xlsx, sheet: prompts
#   - objective_scores_core360.xlsx, sheet: image_scores
#   - objective_scores_extended1440.xlsx, sheet: image_scores
#
# Outputs written to <project_root>/data/rq3_outputs:
#   - one compact Excel workbook with key result tables
#   - one README_RQ3_outputs.md
#   - figures in PNG and PDF under the figures subdirectory only
#
# Notes:
#   This analysis uses objective-metric scores only. It does not use
#   subjective ratings and must not be interpreted as direct evidence
#   of accuracy relative to human judgment.
############################################################

options(stringsAsFactors = FALSE)

# -----------------------------
# 0. Package preparation
# -----------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "writexl",
  "stringr", "purrr", "tibble", "scales", "forcats"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, install_if_missing))
invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------
# 1. Path configuration
# -----------------------------
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  return(getwd())
}

script_dir <- get_script_dir()
project_root <- dirname(dirname(script_dir))
data_dir <- file.path(project_root, "data")
output_dir <- file.path(data_dir, "rq3_outputs")
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

prompt_file <- file.path(data_dir, "prompts.xlsx")
core_file <- file.path(data_dir, "objective_scores_core360.xlsx")
extended_file <- file.path(data_dir, "objective_scores_extended1440.xlsx")

# Clean previous RQ3 output files produced by earlier versions of this script.
# The directory structure is not changed: outputs stay in output_dir, and figures stay in output_dir/figures.
clean_previous_outputs <- TRUE
if (isTRUE(clean_previous_outputs)) {
  old_root_outputs <- list.files(
    output_dir,
    pattern = "^rq3_.*\\.(csv|xlsx)$|^README_RQ3_outputs\\.md$",
    full.names = TRUE
  )
  old_figure_outputs <- list.files(
    figure_dir,
    pattern = "^rq3_.*\\.(png|pdf)$",
    full.names = TRUE
  )
  unlink(c(old_root_outputs, old_figure_outputs), force = TRUE)
}

# -----------------------------
# 2. Helper functions
# -----------------------------
normalize_language <- function(x) {
  x0 <- str_trim(as.character(x))
  dplyr::case_when(
    str_to_lower(x0) %in% c("cn", "zh", "zh-cn", "chinese", "中文") ~ "Chinese",
    str_to_lower(x0) %in% c("en", "eng", "english", "英文") ~ "English",
    TRUE ~ x0
  )
}

normalize_prompt_id <- function(x) {
  str_trim(as.character(x))
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

save_figure <- function(plot_obj, basename, width = 11, height = 7, dpi = 300) {
  png_path <- file.path(figure_dir, paste0(basename, ".png"))
  pdf_path <- file.path(figure_dir, paste0(basename, ".pdf"))
  ggsave(png_path, plot_obj, width = width, height = height, dpi = dpi)
  ggsave(pdf_path, plot_obj, width = width, height = height)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_cor_value <- function(x, y, method = "spearman") {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = method))
}

safe_cor_test <- function(x, y, method = "spearman") {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) {
    return(tibble(correlation = NA_real_, p_value = NA_real_, n = sum(ok)))
  }
  res <- tryCatch(
    suppressWarnings(cor.test(x[ok], y[ok], method = method, exact = FALSE)),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(tibble(correlation = NA_real_, p_value = NA_real_, n = sum(ok)))
  }
  tibble(correlation = unname(res$estimate), p_value = res$p.value, n = sum(ok))
}

safe_wilcox_paired <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  diff_values <- y[ok] - x[ok]
  if (all(abs(diff_values) < .Machine$double.eps, na.rm = TRUE)) return(1)
  res <- tryCatch(
    suppressWarnings(wilcox.test(y[ok], x[ok], paired = TRUE, exact = FALSE)),
    error = function(e) NULL
  )
  if (is.null(res)) return(NA_real_)
  res$p.value
}

rank_models <- function(df) {
  df %>%
    group_by(Dataset, Language, ObjectiveMetric) %>%
    mutate(
      ModelRank = min_rank(desc(MeanScore)),
      RankLabel = paste0(ModelShortName, " (", sprintf("%.3f", MeanScore), ")")
    ) %>%
    ungroup()
}

# -----------------------------
# Publication-style plotting helpers
# -----------------------------
sci_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#F2F2F2", color = NA),
      strip.text = element_text(face = "bold"),
      axis.text = element_text(color = "#333333"),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}

# -----------------------------
# 3. Input validation and loading
# -----------------------------
input_files <- c(prompt_file, core_file, extended_file)
missing_files <- input_files[!file.exists(input_files)]
if (length(missing_files) > 0) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"))
}

prompts_raw <- readxl::read_excel(prompt_file, sheet = "prompts")
core_raw <- readxl::read_excel(core_file, sheet = "image_scores")
extended_raw <- readxl::read_excel(extended_file, sheet = "image_scores")

required_prompt_cols <- c(
  "prompt_id", "set_type", "prompt_cn", "prompt_en", "primary_category",
  "complexity_level", "style_type", "culture_specific",
  "text_symbol_requirement", "notes"
)

required_score_cols <- c(
  "ModelShortName", "PromptID", "Language", "Resolution", "Steps", "SeedNo",
  "OpenCLIPScore", "CNCLIPScore", "AestheticScore", "MUSIQScore",
  "ImageRewardScore", "HPSv2.1Score", "VQAScore", "ImageSizeBytes",
  "ElapsedSeconds", "IterPerSecond", "VRAMUsage", "GPUUsage",
  "MemoryUsage", "CPUUsage", "FileName"
)

missing_prompt_cols <- setdiff(required_prompt_cols, names(prompts_raw))
missing_core_cols <- setdiff(required_score_cols, names(core_raw))
missing_extended_cols <- setdiff(required_score_cols, names(extended_raw))

if (length(missing_prompt_cols) > 0) {
  stop("Missing columns in prompts.xlsx: ", paste(missing_prompt_cols, collapse = ", "))
}
if (length(missing_core_cols) > 0) {
  stop("Missing columns in objective_scores_core360.xlsx: ", paste(missing_core_cols, collapse = ", "))
}
if (length(missing_extended_cols) > 0) {
  stop("Missing columns in objective_scores_extended1440.xlsx: ", paste(missing_extended_cols, collapse = ", "))
}

prompts <- prompts_raw %>%
  mutate(
    PromptID = normalize_prompt_id(prompt_id),
    set_type = as.character(set_type),
    primary_category = as.character(primary_category),
    complexity_level = as.character(complexity_level),
    style_type = as.character(style_type),
    culture_specific = as.character(culture_specific),
    text_symbol_requirement = as.character(text_symbol_requirement)
  ) %>%
  select(
    PromptID, prompt_id, set_type, prompt_cn, prompt_en, primary_category,
    complexity_level, style_type, culture_specific, text_symbol_requirement, notes
  )

prepare_score_data <- function(df, dataset_label) {
  df %>%
    mutate(
      Dataset = dataset_label,
      PromptID = normalize_prompt_id(PromptID),
      Language = normalize_language(Language),
      ModelShortName = as.character(ModelShortName),
      Resolution = as.character(Resolution),
      SeedNo = as.character(SeedNo)
    ) %>%
    rename(
      ImageReward = ImageRewardScore,
      HPSv21 = `HPSv2.1Score`
    ) %>%
    mutate(
      across(
        c(OpenCLIPScore, CNCLIPScore, AestheticScore, MUSIQScore, ImageReward, HPSv21, VQAScore,
          ImageSizeBytes, ElapsedSeconds, IterPerSecond, VRAMUsage, GPUUsage, MemoryUsage, CPUUsage),
        ~ suppressWarnings(as.numeric(.x))
      )
    )
}

core_scores <- prepare_score_data(core_raw, "Core360")
extended_scores <- prepare_score_data(extended_raw, "Extended1440")

all_scores <- bind_rows(core_scores, extended_scores) %>%
  left_join(prompts, by = "PromptID")

metric_cols <- c(
  "OpenCLIPScore", "CNCLIPScore", "AestheticScore", "MUSIQScore",
  "ImageReward", "HPSv21", "VQAScore"
)

metric_labels <- tibble(
  ObjectiveMetric = metric_cols,
  MetricLabel = c("OpenCLIP", "CNCLIP", "Aesthetic", "MUSIQ", "ImageReward", "HPS v2.1", "VQAScore")
)

model_levels <- sort(unique(as.character(all_scores$ModelShortName)))
model_palette <- setNames(c("#0072B2", "#D55E00", "#009E73")[seq_along(model_levels)], model_levels)
model_shape_values <- setNames(c(16, 17, 15)[seq_along(model_levels)], model_levels)
model_linetype_values <- setNames(c("solid", "dashed", "dotdash")[seq_along(model_levels)], model_levels)
dataset_palette <- c("Core360" = "#4DBBD5", "Extended1440" = "#00A087")
rank_palette <- c("1" = "#0B4F6C", "2" = "#5FA8D3", "3" = "#CFE8F3")
diverging_palette <- c("#3B4CC0", "#8DB0FE", "#F7F7F7", "#F4987A", "#B40426")

# Row-level standardized multi-metric mean for descriptive visualization only.
# It is not treated as a validated human-aligned quality score.
metric_means <- sapply(all_scores[metric_cols], mean, na.rm = TRUE)
metric_sds <- sapply(all_scores[metric_cols], sd, na.rm = TRUE)

all_scores <- all_scores %>%
  mutate(
    across(
      all_of(metric_cols),
      ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
      .names = "z_{.col}"
    ),
    ObjectiveMeanZ = rowMeans(across(paste0("z_", metric_cols)), na.rm = TRUE)
  )

long_scores <- all_scores %>%
  select(
    Dataset, ModelShortName, PromptID, Language, Resolution, Steps, SeedNo, FileName,
    set_type, primary_category, complexity_level, style_type, culture_specific,
    text_symbol_requirement, ObjectiveMeanZ,
    all_of(metric_cols)
  ) %>%
  pivot_longer(
    cols = all_of(metric_cols),
    names_to = "ObjectiveMetric",
    values_to = "MetricValue"
  ) %>%
  left_join(metric_labels, by = "ObjectiveMetric") %>%
  mutate(
    ObjectiveMetric = factor(ObjectiveMetric, levels = metric_cols),
    MetricLabel = factor(MetricLabel, levels = metric_labels$MetricLabel),
    Dataset = factor(Dataset, levels = c("Core360", "Extended1440"))
  )

# -----------------------------
# 4. Data overview and validation
# -----------------------------
data_overview <- all_scores %>%
  group_by(Dataset, ModelShortName, Language) %>%
  summarise(
    ImageCount = n(),
    PromptCount = n_distinct(PromptID),
    SeedCount = n_distinct(SeedNo),
    MissingPromptMetadata = sum(is.na(set_type)),
    .groups = "drop"
  ) %>%
  arrange(Dataset, ModelShortName, Language)

prompt_metadata_check <- all_scores %>%
  group_by(Dataset) %>%
  summarise(
    ImageCount = n(),
    MatchedPromptMetadata = sum(!is.na(set_type)),
    MissingPromptMetadata = sum(is.na(set_type)),
    UniquePrompts = n_distinct(PromptID),
    UniquePromptCategories = n_distinct(primary_category, na.rm = TRUE),
    UniqueComplexityLevels = n_distinct(complexity_level, na.rm = TRUE),
    UniqueStyleTypes = n_distinct(style_type, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 5. Metric distribution summaries
# -----------------------------
metric_distribution_summary <- long_scores %>%
  group_by(Dataset, ObjectiveMetric, MetricLabel) %>%
  summarise(
    N = sum(!is.na(MetricValue)),
    Mean = mean(MetricValue, na.rm = TRUE),
    SD = safe_sd(MetricValue),
    Median = median(MetricValue, na.rm = TRUE),
    Q1 = quantile(MetricValue, 0.25, na.rm = TRUE, names = FALSE),
    Q3 = quantile(MetricValue, 0.75, na.rm = TRUE, names = FALSE),
    IQR = IQR(MetricValue, na.rm = TRUE),
    Min = min(MetricValue, na.rm = TRUE),
    Max = max(MetricValue, na.rm = TRUE),
    CV = ifelse(abs(Mean) < .Machine$double.eps, NA_real_, SD / abs(Mean)),
    .groups = "drop"
  ) %>%
  arrange(Dataset, ObjectiveMetric)

metric_distribution_by_model_language <- long_scores %>%
  group_by(Dataset, ModelShortName, Language, ObjectiveMetric, MetricLabel) %>%
  summarise(
    N = sum(!is.na(MetricValue)),
    Mean = mean(MetricValue, na.rm = TRUE),
    SD = safe_sd(MetricValue),
    Median = median(MetricValue, na.rm = TRUE),
    Q1 = quantile(MetricValue, 0.25, na.rm = TRUE, names = FALSE),
    Q3 = quantile(MetricValue, 0.75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  arrange(Dataset, ModelShortName, Language, ObjectiveMetric)

# -----------------------------
# 6. Inter-metric correlations and stability
# -----------------------------
compute_metric_correlations <- function(df, group_vars = c("Dataset")) {
  groups <- df %>% distinct(across(all_of(group_vars)))
  metric_pairs <- combn(metric_cols, 2, simplify = FALSE)
  pmap_dfr(groups, function(...) {
    group_values <- list(...)
    names(group_values) <- group_vars
    sub_df <- df
    for (gv in group_vars) {
      sub_df <- sub_df %>% filter(.data[[gv]] == group_values[[gv]])
    }
    map_dfr(metric_pairs, function(pair) {
      res <- safe_cor_test(sub_df[[pair[1]]], sub_df[[pair[2]]], method = "spearman")
      tibble(
        Metric1 = pair[1],
        Metric2 = pair[2],
        SpearmanRho = res$correlation,
        PValue = res$p_value,
        N = res$n
      ) %>% bind_cols(as_tibble(group_values))
    })
  }) %>%
    relocate(all_of(group_vars), .before = Metric1) %>%
    mutate(PAdjBH = p.adjust(PValue, method = "BH")) %>%
    left_join(metric_labels, by = c("Metric1" = "ObjectiveMetric")) %>%
    rename(Metric1Label = MetricLabel) %>%
    left_join(metric_labels, by = c("Metric2" = "ObjectiveMetric")) %>%
    rename(Metric2Label = MetricLabel)
}

metric_correlation_long <- bind_rows(
  compute_metric_correlations(all_scores, c("Dataset")) %>% mutate(GroupLevel = "Dataset"),
  compute_metric_correlations(all_scores, c("Dataset", "Language")) %>% mutate(GroupLevel = "Dataset_Language"),
  compute_metric_correlations(all_scores, c("Dataset", "ModelShortName")) %>% mutate(GroupLevel = "Dataset_Model"),
  compute_metric_correlations(all_scores, c("Dataset", "ModelShortName", "Language")) %>% mutate(GroupLevel = "Dataset_Model_Language")
)

correlation_stability <- metric_correlation_long %>%
  filter(GroupLevel %in% c("Dataset", "Dataset_Language", "Dataset_Model")) %>%
  select(GroupLevel, Dataset, ModelShortName, Language, Metric1, Metric2, SpearmanRho) %>%
  pivot_wider(names_from = Dataset, values_from = SpearmanRho) %>%
  group_by(GroupLevel, ModelShortName, Language) %>%
  summarise(
    MetricPairCount = sum(complete.cases(Core360, Extended1440)),
    CoreExtendedCorrelation = safe_cor_value(Core360, Extended1440, method = "spearman"),
    MeanAbsoluteDifference = mean(abs(Extended1440 - Core360), na.rm = TRUE),
    MaxAbsoluteDifference = max(abs(Extended1440 - Core360), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(GroupLevel, ModelShortName, Language)

# Matrix-form correlations for heatmap figures.
metric_correlation_matrix_values <- long_scores %>%
  select(Dataset, ObjectiveMetric, MetricLabel, MetricValue, FileName) %>%
  distinct() %>%
  pivot_wider(names_from = ObjectiveMetric, values_from = MetricValue)

cor_heatmap_data <- map_dfr(levels(long_scores$Dataset), function(ds) {
  sub_df <- all_scores %>% filter(Dataset == ds)
  expand_grid(Metric1 = metric_cols, Metric2 = metric_cols) %>%
    mutate(
      SpearmanRho = map2_dbl(Metric1, Metric2, ~ safe_cor_value(sub_df[[.x]], sub_df[[.y]], method = "spearman")),
      Dataset = ds
    )
}) %>%
  left_join(metric_labels, by = c("Metric1" = "ObjectiveMetric")) %>%
  rename(Metric1Label = MetricLabel) %>%
  left_join(metric_labels, by = c("Metric2" = "ObjectiveMetric")) %>%
  rename(Metric2Label = MetricLabel) %>%
  mutate(
    Metric1Label = factor(Metric1Label, levels = metric_labels$MetricLabel),
    Metric2Label = factor(Metric2Label, levels = rev(metric_labels$MetricLabel))
  )

# -----------------------------
# 7. Language sensitivity analysis
# -----------------------------
language_shift_by_model <- long_scores %>%
  select(Dataset, ModelShortName, PromptID, SeedNo, ObjectiveMetric, MetricLabel, Language, MetricValue) %>%
  filter(Language %in% c("Chinese", "English")) %>%
  pivot_wider(names_from = Language, values_from = MetricValue) %>%
  filter(!is.na(Chinese), !is.na(English)) %>%
  mutate(LanguageShift_EN_minus_CN = English - Chinese) %>%
  group_by(Dataset, ModelShortName, ObjectiveMetric, MetricLabel) %>%
  summarise(
    PairCount = n(),
    MeanChinese = mean(Chinese, na.rm = TRUE),
    MeanEnglish = mean(English, na.rm = TRUE),
    MeanShift = mean(LanguageShift_EN_minus_CN, na.rm = TRUE),
    SDShift = safe_sd(LanguageShift_EN_minus_CN),
    MedianShift = median(LanguageShift_EN_minus_CN, na.rm = TRUE),
    Q1Shift = quantile(LanguageShift_EN_minus_CN, 0.25, na.rm = TRUE, names = FALSE),
    Q3Shift = quantile(LanguageShift_EN_minus_CN, 0.75, na.rm = TRUE, names = FALSE),
    AbsMeanShift = abs(MeanShift),
    WilcoxonP = safe_wilcox_paired(Chinese, English),
    .groups = "drop"
  ) %>%
  group_by(Dataset, ObjectiveMetric) %>%
  mutate(PAdjBH_within_dataset_metric = p.adjust(WilcoxonP, method = "BH")) %>%
  ungroup() %>%
  arrange(Dataset, ObjectiveMetric, ModelShortName)

language_shift_overall <- long_scores %>%
  select(Dataset, PromptID, SeedNo, ModelShortName, ObjectiveMetric, MetricLabel, Language, MetricValue) %>%
  filter(Language %in% c("Chinese", "English")) %>%
  pivot_wider(names_from = Language, values_from = MetricValue) %>%
  filter(!is.na(Chinese), !is.na(English)) %>%
  mutate(LanguageShift_EN_minus_CN = English - Chinese) %>%
  group_by(Dataset, ObjectiveMetric, MetricLabel) %>%
  summarise(
    PairCount = n(),
    MeanChinese = mean(Chinese, na.rm = TRUE),
    MeanEnglish = mean(English, na.rm = TRUE),
    MeanShift = mean(LanguageShift_EN_minus_CN, na.rm = TRUE),
    SDShift = safe_sd(LanguageShift_EN_minus_CN),
    MedianShift = median(LanguageShift_EN_minus_CN, na.rm = TRUE),
    Q1Shift = quantile(LanguageShift_EN_minus_CN, 0.25, na.rm = TRUE, names = FALSE),
    Q3Shift = quantile(LanguageShift_EN_minus_CN, 0.75, na.rm = TRUE, names = FALSE),
    AbsMeanShift = abs(MeanShift),
    WilcoxonP = safe_wilcox_paired(Chinese, English),
    .groups = "drop"
  ) %>%
  group_by(Dataset) %>%
  mutate(PAdjBH = p.adjust(WilcoxonP, method = "BH")) %>%
  ungroup() %>%
  arrange(Dataset, ObjectiveMetric)

language_shift_by_prompt_attribute <- long_scores %>%
  select(
    Dataset, PromptID, SeedNo, ModelShortName, ObjectiveMetric, MetricLabel,
    Language, MetricValue, primary_category, complexity_level, style_type,
    culture_specific, text_symbol_requirement
  ) %>%
  filter(Language %in% c("Chinese", "English")) %>%
  pivot_wider(names_from = Language, values_from = MetricValue) %>%
  filter(!is.na(Chinese), !is.na(English)) %>%
  mutate(LanguageShift_EN_minus_CN = English - Chinese) %>%
  pivot_longer(
    cols = c(primary_category, complexity_level, style_type, culture_specific, text_symbol_requirement),
    names_to = "PromptAttribute",
    values_to = "AttributeValue"
  ) %>%
  filter(!is.na(AttributeValue), AttributeValue != "") %>%
  group_by(Dataset, PromptAttribute, AttributeValue, ObjectiveMetric, MetricLabel) %>%
  summarise(
    PairCount = n(),
    MeanShift = mean(LanguageShift_EN_minus_CN, na.rm = TRUE),
    SDShift = safe_sd(LanguageShift_EN_minus_CN),
    MedianShift = median(LanguageShift_EN_minus_CN, na.rm = TRUE),
    AbsMeanShift = abs(MeanShift),
    .groups = "drop"
  ) %>%
  arrange(Dataset, PromptAttribute, AttributeValue, ObjectiveMetric)

# -----------------------------
# 8. Model ranking and ranking stability
# -----------------------------
model_ranking_summary <- long_scores %>%
  group_by(Dataset, Language, ModelShortName, ObjectiveMetric, MetricLabel) %>%
  summarise(
    N = sum(!is.na(MetricValue)),
    MeanScore = mean(MetricValue, na.rm = TRUE),
    SDScore = safe_sd(MetricValue),
    MedianScore = median(MetricValue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rank_models() %>%
  arrange(Dataset, Language, ObjectiveMetric, ModelRank)

model_rank_stability <- model_ranking_summary %>%
  select(Dataset, Language, ObjectiveMetric, MetricLabel, ModelShortName, ModelRank, MeanScore) %>%
  pivot_wider(
    names_from = Dataset,
    values_from = c(ModelRank, MeanScore),
    names_sep = "_"
  ) %>%
  group_by(Language, ObjectiveMetric, MetricLabel) %>%
  summarise(
    ModelCount = sum(complete.cases(ModelRank_Core360, ModelRank_Extended1440)),
    RankSpearman_Core_vs_Extended = safe_cor_value(ModelRank_Core360, ModelRank_Extended1440, method = "spearman"),
    MeanRankAbsDiff = mean(abs(ModelRank_Extended1440 - ModelRank_Core360), na.rm = TRUE),
    RankChangedModels = sum(ModelRank_Core360 != ModelRank_Extended1440, na.rm = TRUE),
    MeanScoreSpearman_Core_vs_Extended = safe_cor_value(MeanScore_Core360, MeanScore_Extended1440, method = "spearman"),
    .groups = "drop"
  ) %>%
  arrange(Language, ObjectiveMetric)

model_ranking_overall <- long_scores %>%
  group_by(Dataset, ModelShortName, ObjectiveMetric, MetricLabel) %>%
  summarise(
    N = sum(!is.na(MetricValue)),
    MeanScore = mean(MetricValue, na.rm = TRUE),
    SDScore = safe_sd(MetricValue),
    .groups = "drop"
  ) %>%
  group_by(Dataset, ObjectiveMetric, MetricLabel) %>%
  mutate(ModelRank = min_rank(desc(MeanScore))) %>%
  ungroup() %>%
  arrange(Dataset, ObjectiveMetric, ModelRank)

# -----------------------------
# 9. Prompt attribute analysis
# -----------------------------
prompt_attribute_distribution <- all_scores %>%
  select(Dataset, PromptID, primary_category, complexity_level, style_type, culture_specific, text_symbol_requirement) %>%
  distinct() %>%
  pivot_longer(
    cols = c(primary_category, complexity_level, style_type, culture_specific, text_symbol_requirement),
    names_to = "PromptAttribute",
    values_to = "AttributeValue"
  ) %>%
  filter(!is.na(AttributeValue), AttributeValue != "") %>%
  group_by(Dataset, PromptAttribute, AttributeValue) %>%
  summarise(
    PromptCount = n_distinct(PromptID),
    .groups = "drop"
  ) %>%
  arrange(Dataset, PromptAttribute, desc(PromptCount), AttributeValue)

prompt_attribute_metric_summary <- long_scores %>%
  pivot_longer(
    cols = c(primary_category, complexity_level, style_type, culture_specific, text_symbol_requirement),
    names_to = "PromptAttribute",
    values_to = "AttributeValue"
  ) %>%
  filter(!is.na(AttributeValue), AttributeValue != "") %>%
  group_by(Dataset, PromptAttribute, AttributeValue, ObjectiveMetric, MetricLabel) %>%
  summarise(
    ImageCount = sum(!is.na(MetricValue)),
    PromptCount = n_distinct(PromptID),
    Mean = mean(MetricValue, na.rm = TRUE),
    SD = safe_sd(MetricValue),
    Median = median(MetricValue, na.rm = TRUE),
    Q1 = quantile(MetricValue, 0.25, na.rm = TRUE, names = FALSE),
    Q3 = quantile(MetricValue, 0.75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  arrange(Dataset, PromptAttribute, AttributeValue, ObjectiveMetric)

prompt_attribute_objective_mean_z <- all_scores %>%
  select(Dataset, PromptID, ModelShortName, Language, ObjectiveMeanZ,
         primary_category, complexity_level, style_type, culture_specific, text_symbol_requirement) %>%
  pivot_longer(
    cols = c(primary_category, complexity_level, style_type, culture_specific, text_symbol_requirement),
    names_to = "PromptAttribute",
    values_to = "AttributeValue"
  ) %>%
  filter(!is.na(AttributeValue), AttributeValue != "") %>%
  group_by(Dataset, PromptAttribute, AttributeValue) %>%
  summarise(
    ImageCount = sum(!is.na(ObjectiveMeanZ)),
    PromptCount = n_distinct(PromptID),
    MeanObjectiveMeanZ = mean(ObjectiveMeanZ, na.rm = TRUE),
    SDObjectiveMeanZ = safe_sd(ObjectiveMeanZ),
    MedianObjectiveMeanZ = median(ObjectiveMeanZ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Dataset, PromptAttribute, AttributeValue)

# -----------------------------
# 10. Resource summary for later practical applicability analysis
# -----------------------------
resource_cols <- c("ElapsedSeconds", "IterPerSecond", "VRAMUsage", "GPUUsage", "MemoryUsage", "CPUUsage", "ImageSizeBytes")

resource_summary <- all_scores %>%
  group_by(Dataset, ModelShortName, Language) %>%
  summarise(
    N = n(),
    MeanElapsedSeconds = mean(ElapsedSeconds, na.rm = TRUE),
    SDElapsedSeconds = safe_sd(ElapsedSeconds),
    MedianElapsedSeconds = median(ElapsedSeconds, na.rm = TRUE),
    MeanIterPerSecond = mean(IterPerSecond, na.rm = TRUE),
    MeanVRAMUsage = mean(VRAMUsage, na.rm = TRUE),
    MeanGPUUsage = mean(GPUUsage, na.rm = TRUE),
    MeanMemoryUsage = mean(MemoryUsage, na.rm = TRUE),
    MeanCPUUsage = mean(CPUUsage, na.rm = TRUE),
    MeanImageSizeBytes = mean(ImageSizeBytes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Dataset, ModelShortName, Language)

# -----------------------------
# 11. Paper-ready compact tables
# -----------------------------
table_language_shift_for_paper <- language_shift_overall %>%
  mutate(
    MeanChinese = round(MeanChinese, 3),
    MeanEnglish = round(MeanEnglish, 3),
    MeanShift = round(MeanShift, 3),
    MedianShift = round(MedianShift, 3),
    PAdjBH = format_p(PAdjBH)
  ) %>%
  select(Dataset, MetricLabel, PairCount, MeanChinese, MeanEnglish, MeanShift, MedianShift, PAdjBH)

table_model_rank_stability_for_paper <- model_rank_stability %>%
  mutate(
    RankSpearman_Core_vs_Extended = round(RankSpearman_Core_vs_Extended, 3),
    MeanRankAbsDiff = round(MeanRankAbsDiff, 3),
    MeanScoreSpearman_Core_vs_Extended = round(MeanScoreSpearman_Core_vs_Extended, 3)
  ) %>%
  select(Language, MetricLabel, ModelCount, RankSpearman_Core_vs_Extended,
         MeanRankAbsDiff, RankChangedModels, MeanScoreSpearman_Core_vs_Extended)

table_correlation_stability_for_paper <- correlation_stability %>%
  filter(GroupLevel == "Dataset") %>%
  mutate(
    CoreExtendedCorrelation = round(CoreExtendedCorrelation, 3),
    MeanAbsoluteDifference = round(MeanAbsoluteDifference, 3),
    MaxAbsoluteDifference = round(MaxAbsoluteDifference, 3)
  ) %>%
  select(GroupLevel, MetricPairCount, CoreExtendedCorrelation, MeanAbsoluteDifference, MaxAbsoluteDifference)

table_prompt_attribute_for_paper <- prompt_attribute_objective_mean_z %>%
  mutate(
    MeanObjectiveMeanZ = round(MeanObjectiveMeanZ, 3),
    SDObjectiveMeanZ = round(SDObjectiveMeanZ, 3),
    MedianObjectiveMeanZ = round(MedianObjectiveMeanZ, 3)
  ) %>%
  select(Dataset, PromptAttribute, AttributeValue, PromptCount, ImageCount,
         MeanObjectiveMeanZ, SDObjectiveMeanZ, MedianObjectiveMeanZ)

# Compact prompt-attribute contrast table.
# This keeps the most interpretable attribute-level signal instead of exporting all detailed groups.
table_prompt_attribute_contrast_for_paper <- prompt_attribute_objective_mean_z %>%
  filter(!is.na(MeanObjectiveMeanZ)) %>%
  group_by(Dataset, PromptAttribute) %>%
  summarise(
    AttributeLevelCount = n(),
    MaxAttributeValue = AttributeValue[which.max(MeanObjectiveMeanZ)][1],
    MaxMeanObjectiveMeanZ = max(MeanObjectiveMeanZ, na.rm = TRUE),
    MinAttributeValue = AttributeValue[which.min(MeanObjectiveMeanZ)][1],
    MinMeanObjectiveMeanZ = min(MeanObjectiveMeanZ, na.rm = TRUE),
    RangeMeanObjectiveMeanZ = MaxMeanObjectiveMeanZ - MinMeanObjectiveMeanZ,
    SmallestPromptCount = min(PromptCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MaxMeanObjectiveMeanZ = round(MaxMeanObjectiveMeanZ, 3),
    MinMeanObjectiveMeanZ = round(MinMeanObjectiveMeanZ, 3),
    RangeMeanObjectiveMeanZ = round(RangeMeanObjectiveMeanZ, 3)
  ) %>%
  arrange(Dataset, desc(RangeMeanObjectiveMeanZ), PromptAttribute)

# -----------------------------
# 12. Figures
# -----------------------------
# Figure 1: Model metric profiles based on standardized metric values.
# This is more suitable for the main text than exporting full distribution plots.
standardized_long <- all_scores %>%
  select(Dataset, ModelShortName, Language, PromptID, starts_with("z_")) %>%
  pivot_longer(
    cols = starts_with("z_"),
    names_to = "ObjectiveMetric",
    values_to = "MetricZ"
  ) %>%
  mutate(ObjectiveMetric = str_remove(ObjectiveMetric, "^z_")) %>%
  left_join(metric_labels, by = "ObjectiveMetric") %>%
  mutate(MetricLabel = factor(MetricLabel, levels = metric_labels$MetricLabel))

model_metric_profile <- standardized_long %>%
  group_by(Dataset, Language, ModelShortName, MetricLabel) %>%
  summarise(
    N = sum(!is.na(MetricZ)),
    MeanMetricZ = mean(MetricZ, na.rm = TRUE),
    SEMetricZ = safe_sd(MetricZ) / sqrt(N),
    .groups = "drop"
  )

fig_model_metric_profiles <- ggplot(
  model_metric_profile,
  aes(
    x = MetricLabel,
    y = MeanMetricZ,
    group = ModelShortName,
    color = ModelShortName,
    shape = ModelShortName,
    linetype = ModelShortName
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#7A7A7A") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  facet_grid(Dataset ~ Language) +
  scale_color_manual(values = model_palette) +
  scale_shape_manual(values = model_shape_values) +
  scale_linetype_manual(values = model_linetype_values) +
  labs(
    title = "Standardized objective-metric profiles by generation source, prompt set, and language",
    x = "Objective metric",
    y = "Mean standardized metric score",
    color = "Generation source",
    shape = "Generation source",
    linetype = "Generation source"
  ) +
  sci_theme(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
save_figure(fig_model_metric_profiles, "rq3_model_metric_profiles", width = 13, height = 8)

# Figure 2: Correlation heatmap for core vs extended objective-only sets.
fig_cor_heatmap <- ggplot(cor_heatmap_data, aes(x = Metric1Label, y = Metric2Label, fill = SpearmanRho)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", SpearmanRho)), size = 2.8) +
  facet_wrap(~ Dataset) +
  scale_fill_gradientn(
    colours = diverging_palette,
    values = scales::rescale(c(-1, -0.4, 0, 0.4, 1)),
    limits = c(-1, 1),
    name = "Spearman rho"
  ) +
  labs(
    title = "Inter-metric Spearman correlations in Core360 and Extended1440",
    x = "Metric",
    y = "Metric"
  ) +
  sci_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(fig_cor_heatmap, "rq3_metric_correlation_heatmap", width = 12, height = 6.5)

# Figure 3: Language shift heatmap by dataset, model, and metric.
language_shift_limit <- max(abs(language_shift_by_model$MeanShift), na.rm = TRUE)
fig_language_shift <- language_shift_by_model %>%
  mutate(
    MetricLabel = factor(MetricLabel, levels = metric_labels$MetricLabel),
    ModelShortName = factor(ModelShortName, levels = model_levels)
  ) %>%
  ggplot(aes(x = MetricLabel, y = ModelShortName, fill = MeanShift)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", MeanShift)), size = 2.7) +
  facet_wrap(~ Dataset) +
  scale_fill_gradientn(
    colours = diverging_palette,
    values = scales::rescale(c(-language_shift_limit, -language_shift_limit / 2, 0, language_shift_limit / 2, language_shift_limit)),
    limits = c(-language_shift_limit, language_shift_limit),
    name = "EN - CN"
  ) +
  labs(
    title = "Mean prompt-language shift in objective metrics (English minus Chinese)",
    x = "Objective metric",
    y = "Generation source"
  ) +
  sci_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(fig_language_shift, "rq3_language_shift_heatmap", width = 12, height = 6.5)

# Figure 4: Model ranking heatmap.
fig_model_ranking <- model_ranking_summary %>%
  mutate(
    MetricLabel = factor(MetricLabel, levels = metric_labels$MetricLabel),
    ModelShortName = factor(ModelShortName, levels = model_levels),
    ModelRankFactor = factor(ModelRank, levels = c(1, 2, 3))
  ) %>%
  ggplot(aes(x = MetricLabel, y = ModelShortName, fill = ModelRankFactor)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ModelRank), size = 3) +
  facet_grid(Dataset ~ Language) +
  scale_fill_manual(values = rank_palette, drop = FALSE, name = "Rank") +
  labs(
    title = "Metric-specific generation-source ordering across prompt sets and languages",
    x = "Objective metric",
    y = "Generation source"
  ) +
  sci_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(fig_model_ranking, "rq3_model_ranking_heatmap", width = 13, height = 8)

# Figure 5: Metric-specific generation-source ordering across prompt sets and languages.
# This publication-oriented slopegraph directly shows whether model ordering is preserved
# from Core360 to Extended1440 for each metric under Chinese and English prompts.
fig_generation_source_ordering <- model_ranking_summary %>%
  mutate(
    Dataset = factor(Dataset, levels = c("Core360", "Extended1440")),
    Language = factor(Language, levels = c("Chinese", "English")),
    MetricLabel = factor(MetricLabel, levels = metric_labels$MetricLabel),
    ModelShortName = factor(ModelShortName, levels = model_levels)
  ) %>%
  ggplot(aes(x = Dataset, y = ModelRank, group = ModelShortName, color = ModelShortName)) +
  geom_line(linewidth = 0.8, alpha = 0.95) +
  geom_point(size = 2.2) +
  facet_grid(Language ~ MetricLabel) +
  scale_color_manual(values = model_palette, name = "Generation source") +
  scale_y_reverse(
    breaks = c(1, 2, 3),
    limits = c(3.15, 0.85),
    labels = c("1", "2", "3")
  ) +
  labs(
    title = "Metric-specific generation-source ordering across prompt sets and languages",
    subtitle = "Horizontal lines indicate stable ordering from Core360 to Extended1440; crossings indicate ordering changes.",
    x = "Prompt set",
    y = "Generation-source rank (1 = highest mean score)"
  ) +
  sci_theme(base_size = 11) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )
save_figure(fig_generation_source_ordering, "rq3_generation_source_ordering", width = 15, height = 6.8)

# Figure 6: Compact prompt-attribute contrast.
# This appendix figure shows the range of standardized multi-metric means across attribute levels.
fig_prompt_attribute_contrast <- table_prompt_attribute_contrast_for_paper %>%
  mutate(PromptAttribute = fct_reorder(PromptAttribute, RangeMeanObjectiveMeanZ, .fun = mean, .desc = TRUE)) %>%
  ggplot(aes(x = PromptAttribute, y = RangeMeanObjectiveMeanZ, fill = Dataset)) +
  geom_col(width = 0.7) +
  facet_wrap(~ Dataset) +
  scale_fill_manual(values = dataset_palette, guide = "none") +
  labs(
    title = "Prompt-attribute sensitivity of objective-metric profiles",
    x = "Prompt attribute",
    y = "Range of mean standardized multi-metric score"
  ) +
  sci_theme(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_figure(fig_prompt_attribute_contrast, "rq3_prompt_attribute_contrast", width = 10, height = 6.5)

# -----------------------------
# 13. Write compact output workbook
# -----------------------------
# Only one Excel workbook is exported to avoid listing many intermediate tables.
# Detailed intermediate objects remain available inside this script if further checks are needed.
workbook_tables <- list(
  data_overview = data_overview,
  prompt_metadata_check = prompt_metadata_check,
  distribution_by_model_language = metric_distribution_by_model_language,
  correlation_stability = correlation_stability,
  language_sensitivity_by_model = language_shift_by_model,
  model_ranking = model_ranking_summary,
  model_rank_stability = model_rank_stability,
  prompt_attribute_contrast = table_prompt_attribute_contrast_for_paper
)

writexl::write_xlsx(workbook_tables, file.path(output_dir, "rq3_key_results.xlsx"))

# -----------------------------
# 14. README output
# -----------------------------
readme_text <- c(
  "# RQ3 输出文件说明",
  "",
  "## 分析目的",
  "本目录保存 RQ3 的核心输出，用于分析客观指标在更大双语提示词空间中的行为稳定性。",
  "分析使用 360 张 Core360 图像、1440 张 Extended1440 图像，以及 prompts.xlsx 中的提示词元数据。",
  "该分析仅用于观察客观指标在扩展提示词空间中的行为，不直接解释为相对于主观感知质量的准确性证据。",
  "",
  "## 主要输入文件",
  "- prompts.xlsx：提示词文本与提示词属性。",
  "- objective_scores_core360.xlsx：Core360 图像的客观指标评分与资源记录。",
  "- objective_scores_extended1440.xlsx：Extended1440 图像的客观指标评分与资源记录。",
  "",
  "## 输出结构",
  "- rq3_key_results.xlsx：RQ3 主要结果工作簿，包含正文写作和附录复核所需的关键表格。",
  "- README_RQ3_outputs.md：本说明文件。",
  "- figures/：图像输出目录，包含 PNG 和 PDF 两种格式。",
  "",
  "## 工作簿工作表",
  "- data_overview：按数据集、生成来源和语言统计图像、提示词与随机种子覆盖情况。",
  "- prompt_metadata_check：提示词元数据匹配与覆盖检查。",
  "- distribution_by_model_language：按数据集、生成来源、语言和指标汇总的分布统计。",
  "- correlation_stability：Core360 与 Extended1440 之间的指标间相关结构稳定性。",
  "- language_sensitivity_by_model：按数据集、生成来源和指标汇总的英文减中文配对差值。",
  "- model_ranking：按数据集、提示词语言和指标得到的生成来源排序。",
  "- model_rank_stability：Core360 与 Extended1440 之间的生成来源排序稳定性。",
  "- prompt_attribute_contrast：基于标准化多指标均值范围的提示词属性差异汇总表。",
  "",
  "## 图像文件",
  "- figures/rq3_model_metric_profiles.png/.pdf：按生成来源、数据集和语言展示标准化客观指标轮廓。",
  "- figures/rq3_language_shift_heatmap.png/.pdf：按数据集、生成来源和指标展示英文减中文平均差值。",
  "- figures/rq3_model_ranking_heatmap.png/.pdf：按指标、数据集和语言展示生成来源排序。",
  "- figures/rq3_metric_correlation_heatmap.png/.pdf：展示指标间 Spearman 相关结构。",
  "- figures/rq3_prompt_attribute_contrast.png/.pdf：展示提示词属性对应的客观指标轮廓差异。",
  "",
  "## 论文使用方式",
  "正文图表可优先使用 rq3_model_metric_profiles、rq3_language_shift_heatmap 和生成来源排序结果。",
  "指标相关热力图和提示词属性差异图可根据篇幅放入附录。",
  "提示词属性结果用于描述客观指标行为差异，不直接解释为主观感知质量差异。",
  ""
)

writeLines(readme_text, file.path(output_dir, "README_RQ3_outputs.md"))

message("RQ3 analysis script finished successfully.")
message("Output folder: ", output_dir)
