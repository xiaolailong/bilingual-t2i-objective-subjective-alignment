required_pkgs <- c(
  "readxl", "dplyr", "stringr", "purrr", "tidyr",
  "lme4", "openxlsx", "tibble"
)

to_install <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(lme4)
library(openxlsx)
library(tibble)

script_dir <- dirname(sys.frame(1)$ofile)
data_dir <- file.path(dirname(dirname(script_dir)), "data")

input_file  <- file.path(data_dir, "subjective_scores_human120.xlsx")
output_file <- file.path(data_dir, "subjective_reliability_human120.xlsx")


reference_k <- 30


clean_image_name <- function(x) {
  x <- trimws(as.character(x))
  extracted <- stringr::str_extract(x, "c_.*\\.png$")
  ifelse(!is.na(extracted), extracted, x)
}

get_varcomp_value <- function(vc_df, grp_name) {
  idx <- which(vc_df$grp == grp_name)
  if (length(idx) == 0) return(NA_real_)
  vc_df$vcov[idx][1]
}

calc_mixed_reliability <- function(dat_q, q_label, ref_k = 30) {
  n_raters_by_image <- dat_q %>%
    distinct(image_name, respondent_uid) %>%
    count(image_name, name = "n_raters")
  
  k_mean <- mean(n_raters_by_image$n_raters, na.rm = TRUE)
  k_min  <- min(n_raters_by_image$n_raters, na.rm = TRUE)
  k_max  <- max(n_raters_by_image$n_raters, na.rm = TRUE)
  k_sd   <- sd(n_raters_by_image$n_raters, na.rm = TRUE)
  
  fit <- lmer(
    score ~ 1 + (1 | image_name) + (1 | respondent_uid),
    data = dat_q,
    REML = TRUE,
    control = lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  )
  
  vc <- as.data.frame(VarCorr(fit))
  var_image <- get_varcomp_value(vc, "image_name")
  var_rater <- get_varcomp_value(vc, "respondent_uid")
  var_resid <- attr(VarCorr(fit), "sc")^2
  
  total_var <- var_image + var_rater + var_resid
  
  rel_single <- var_image / total_var
  
  rel_k_mean <- var_image / (var_image + (var_rater + var_resid) / k_mean)
  rel_k_min  <- var_image / (var_image + (var_rater + var_resid) / k_min)
  rel_k_ref  <- var_image / (var_image + (var_rater + var_resid) / ref_k)
  
  tibble(
    q_no = q_label,
    n_images = n_distinct(dat_q$image_name),
    n_respondents = n_distinct(dat_q$respondent_uid),
    n_ratings = nrow(dat_q),
    
    n_raters_min = k_min,
    n_raters_max = k_max,
    n_raters_mean = k_mean,
    n_raters_sd = k_sd,
    
    var_image = var_image,
    var_rater = var_rater,
    var_residual = var_resid,
    
    reliability_single_rater = rel_single,
    reliability_average_actual_mean_k = rel_k_mean,
    reliability_average_min_k = rel_k_min,
    reliability_average_reference_k = rel_k_ref
  )
}

sheet_names <- excel_sheets(input_file)

raw_list <- lapply(sheet_names, function(sh) {
  df <- read_excel(input_file, sheet = sh)
  
  expected_cols <- c(
    "respondent_id", "duration_seconds", "image_name",
    "prompt_id", "model", "language", "q_no", "score",
    "Content", "Style", "Complexity", "Context"
  )
  missing_cols <- setdiff(expected_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("sheet [%s] Info:%s", sh, paste(missing_cols, collapse = ", ")))
  }
  
  df %>%
    mutate(
      source_sheet = sh,
      respondent_id = as.character(respondent_id),
      respondent_uid = paste0(sh, "::", respondent_id),
      image_name = clean_image_name(image_name),
      q_no = as.character(q_no),
      score = as.numeric(score),
      duration_seconds = as.numeric(duration_seconds)
    )
})

dat <- bind_rows(raw_list)

dat <- dat %>%
  filter(!is.na(image_name), image_name != "") %>%
  filter(!is.na(q_no), q_no %in% c("Q1", "Q2", "Q3")) %>%
  filter(!is.na(score), score >= 1, score <= 5)

dup_check <- dat %>%
  count(respondent_uid, image_name, q_no, name = "n") %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  warning("Duplicate rating records were found for the same respondent_uid + image_name + q_no. Duplicate values will be averaged.")
  
  dat <- dat %>%
    group_by(
      respondent_uid, respondent_id, source_sheet, duration_seconds,
      image_name, prompt_id, model, language, q_no,
      Content, Style, Complexity, Context
    ) %>%
    summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
}

triplet_check <- dat %>%
  distinct(respondent_uid, image_name, q_no) %>%
  count(respondent_uid, image_name, name = "n_q") %>%
  count(n_q)

q_levels <- c("Q1", "Q2", "Q3")

reliability_main <- map_dfr(q_levels, function(qi) {
  dat_q <- dat %>% filter(q_no == qi)
  calc_mixed_reliability(dat_q, qi, ref_k = reference_k)
})

image_rater_counts <- dat %>%
  distinct(q_no, image_name, respondent_uid) %>%
  count(q_no, image_name, name = "n_raters") %>%
  arrange(q_no, image_name)

agreement_results <- tibble()

basic_summary <- dat %>%
  summarise(
    total_rows = n(),
    total_images = n_distinct(image_name),
    total_respondents = n_distinct(respondent_uid),
    total_teacher_rows = sum(source_sheet == "teacher"),
    total_student_rows = sum(source_sheet != "teacher")
  )

sheet_summary <- dat %>%
  group_by(source_sheet) %>%
  summarise(
    n_rows = n(),
    n_respondents = n_distinct(respondent_uid),
    n_images = n_distinct(image_name),
    .groups = "drop"
  )

wb <- createWorkbook()

addWorksheet(wb, "basic_summary")
writeData(wb, "basic_summary", basic_summary)

addWorksheet(wb, "sheet_summary")
writeData(wb, "sheet_summary", sheet_summary)

addWorksheet(wb, "triplet_check")
writeData(wb, "triplet_check", triplet_check)

addWorksheet(wb, "main_reliability")
writeData(wb, "main_reliability", reliability_main)

addWorksheet(wb, "image_rater_counts")
writeData(wb, "image_rater_counts", image_rater_counts)

if (nrow(agreement_results) > 0) {
  addWorksheet(wb, "agreement_metrics")
  writeData(wb, "agreement_metrics", agreement_results)
}

addWorksheet(wb, "cleaned_data_preview")
writeData(wb, "cleaned_data_preview", head(dat, 200))

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Analysis completed; results written to:", output_file, "\n")