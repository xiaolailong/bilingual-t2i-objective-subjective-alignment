import sys
from pathlib import Path

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
TABLES_DIR = PROJECT_ROOT / "data"

SUBJECTIVE_FILE = "subjective_scores_human120_image_level.xlsx"
OBJECTIVE_FILE = "objective_scores_core360.xlsx"
OUTPUT_FILE = "analysis_human_objective_120.xlsx"

SUBJECTIVE_SHEET = "image_level"
OBJECTIVE_SHEET = "image_scores"


OBJECTIVE_METRIC_COLUMNS = [
    "OpenCLIPScore",
    "CNCLIPScore",
    "AestheticScore",
    "MUSIQScore",
    "ImageRewardScore",
    "HPSv2.1Score",
    "VQAScore",
]

OBJECTIVE_GENERATION_COLUMNS = [
    "Resolution",
    "Steps",
    "SeedNo",
    "ImageSizeBytes",
    "ElapsedSeconds",
    "IterPerSecond",
    "VRAMUsage",
    "GPUUsage",
    "MemoryUsage",
    "CPUUsage",
]

SUBJECTIVE_SCORE_COLUMNS = [
    "MeanAdherence",
    "AdherenceSD",
    "MeanClarity",
    "ClaritySD",
    "MeanAesthetics",
    "AestheticsSD",
    "CompositeScore",
    "ValidRaterCount",
]


def normalize_filename(value):
    """
    Normalize image filename for matching.

    This handles cases where one file stores only the filename
    and another stores a relative or absolute path.
    """
    if pd.isna(value):
        return None

    value = str(value).strip().replace("\\", "/")
    return Path(value).name.lower()


def check_required_columns(df, required_columns, table_name):
    """
    Check whether all required columns exist in a DataFrame.
    """
    missing = [col for col in required_columns if col not in df.columns]
    if missing:
        raise ValueError(
            f"{table_name} is missing required columns: {missing}"
        )


def main():
    subjective_path = DATA_DIR / SUBJECTIVE_FILE
    objective_path = DATA_DIR / OBJECTIVE_FILE
    output_path = TABLES_DIR / OUTPUT_FILE

    if not subjective_path.exists():
        raise FileNotFoundError(f"Subjective file not found: {subjective_path}")

    if not objective_path.exists():
        raise FileNotFoundError(f"Objective file not found: {objective_path}")

    TABLES_DIR.mkdir(parents=True, exist_ok=True)

    print("Reading input files...")

    subjective_df = pd.read_excel(
        subjective_path,
        sheet_name=SUBJECTIVE_SHEET,
        engine="openpyxl"
    )

    objective_df = pd.read_excel(
        objective_path,
        sheet_name=OBJECTIVE_SHEET,
        engine="openpyxl"
    )

    print(f"Subjective image-level rows: {len(subjective_df)}")
    print(f"Objective core rows: {len(objective_df)}")

    subjective_required = [
        "ImageName",
        "ModelShortName",
        "PromptID",
        "Language",
        *SUBJECTIVE_SCORE_COLUMNS,
    ]

    objective_required = [
        "FileName",
        "ModelShortName",
        "PromptID",
        "Language",
        *OBJECTIVE_METRIC_COLUMNS,
        *OBJECTIVE_GENERATION_COLUMNS,
    ]

    check_required_columns(subjective_df, subjective_required, "Subjective table")
    check_required_columns(objective_df, objective_required, "Objective table")

    print("Normalizing image filenames...")

    subjective_df["ImageKey"] = subjective_df["ImageName"].apply(normalize_filename)
    objective_df["ImageKey"] = objective_df["FileName"].apply(normalize_filename)

    if subjective_df["ImageKey"].isna().any():
        raise ValueError("Some ImageName values in subjective table cannot be normalized.")

    if objective_df["ImageKey"].isna().any():
        raise ValueError("Some FileName values in objective table cannot be normalized.")

    subjective_duplicate_keys = subjective_df[
        subjective_df.duplicated(subset=["ImageKey"], keep=False)
    ]

    objective_duplicate_keys = objective_df[
        objective_df.duplicated(subset=["ImageKey"], keep=False)
    ]

    if not subjective_duplicate_keys.empty:
        duplicate_output = TABLES_DIR / "diagnostic_subjective_duplicate_image_keys.xlsx"
        subjective_duplicate_keys.to_excel(duplicate_output, index=False)
        raise ValueError(
            f"Duplicate ImageKey values found in subjective table. "
            f"Diagnostic file saved to: {duplicate_output}"
        )

    if not objective_duplicate_keys.empty:
        duplicate_output = TABLES_DIR / "diagnostic_objective_duplicate_image_keys.xlsx"
        objective_duplicate_keys.to_excel(duplicate_output, index=False)
        raise ValueError(
            f"Duplicate ImageKey values found in objective table. "
            f"Diagnostic file saved to: {duplicate_output}"
        )

    print("Selecting and renaming objective columns...")

    objective_selected_columns = [
        "ImageKey",
        "FileName",
        "ModelShortName",
        "PromptID",
        "Language",
        *OBJECTIVE_METRIC_COLUMNS,
        *OBJECTIVE_GENERATION_COLUMNS,
    ]

    objective_selected = objective_df[objective_selected_columns].copy()

    objective_selected = objective_selected.rename(
        columns={
            "FileName": "ObjectiveFileName",
            "ModelShortName": "ObjectiveModelShortName",
            "PromptID": "ObjectivePromptID",
            "Language": "ObjectiveLanguage",
            "ElapsedSeconds": "GenerationElapsedSeconds",
            "IterPerSecond": "GenerationIterPerSecond",
            "VRAMUsage": "GenerationVRAMUsage",
            "GPUUsage": "GenerationGPUUsage",
            "MemoryUsage": "GenerationMemoryUsage",
            "CPUUsage": "GenerationCPUUsage",
            "ImageRewardScore": "ImageReward",
            "HPSv2.1Score": "HPSv21",
        }
    )

    print("Merging subjective and objective data...")

    merged_df = subjective_df.merge(
        objective_selected,
        on="ImageKey",
        how="left",
        validate="one_to_one"
    )

    unmatched_df = merged_df[merged_df["ObjectiveFileName"].isna()].copy()

    if not unmatched_df.empty:
        unmatched_output = TABLES_DIR / "diagnostic_unmatched_subjective_images.xlsx"
        unmatched_df.to_excel(unmatched_output, index=False)
        raise ValueError(
            f"{len(unmatched_df)} subjective images were not matched with objective scores. "
            f"Diagnostic file saved to: {unmatched_output}"
        )

    print("Checking consistency of shared metadata...")

    consistency_checks = []

    shared_checks = [
        ("ModelShortName", "ObjectiveModelShortName"),
        ("PromptID", "ObjectivePromptID"),
        ("Language", "ObjectiveLanguage"),
    ]

    for left_col, right_col in shared_checks:
        mismatch_mask = merged_df[left_col].astype(str) != merged_df[right_col].astype(str)
        mismatch_count = int(mismatch_mask.sum())

        consistency_checks.append(
            {
                "CheckItem": f"{left_col} vs {right_col}",
                "MismatchCount": mismatch_count,
            }
        )

    consistency_df = pd.DataFrame(consistency_checks)

    metadata_mismatch_mask = (
        (merged_df["ModelShortName"].astype(str) != merged_df["ObjectiveModelShortName"].astype(str))
        |
        (merged_df["PromptID"].astype(str) != merged_df["ObjectivePromptID"].astype(str))
        |
        (merged_df["Language"].astype(str) != merged_df["ObjectiveLanguage"].astype(str))
    )

    metadata_mismatch_df = merged_df[metadata_mismatch_mask].copy()

    if not metadata_mismatch_df.empty:
        mismatch_output = TABLES_DIR / "diagnostic_metadata_mismatch.xlsx"
        metadata_mismatch_df.to_excel(mismatch_output, index=False)
        raise ValueError(
            f"Metadata mismatch found after merging. "
            f"Diagnostic file saved to: {mismatch_output}"
        )

    print("Preparing final output columns...")

    # Subjective file also contains ElapsedSeconds and VRAMUsage.
    # To avoid ambiguity, rename them as subjective-side generation metadata.
    rename_subjective_generation = {}
    if "ElapsedSeconds" in merged_df.columns:
        rename_subjective_generation["ElapsedSeconds"] = "SubjectiveFileElapsedSeconds"
    if "VRAMUsage" in merged_df.columns:
        rename_subjective_generation["VRAMUsage"] = "SubjectiveFileVRAMUsage"

    merged_df = merged_df.rename(columns=rename_subjective_generation)

    final_columns = [
        "ImageName",
        "ObjectiveFileName",
        "ImageKey",
        "ModelShortName",
        "PromptID",
        "Language",

        "Resolution",
        "Steps",
        "SeedNo",

        "OpenCLIPScore",
        "CNCLIPScore",
        "AestheticScore",
        "MUSIQScore",
        "ImageReward",
        "HPSv21",
        "VQAScore",

        "MeanAdherence",
        "AdherenceSD",
        "MeanClarity",
        "ClaritySD",
        "MeanAesthetics",
        "AestheticsSD",
        "CompositeScore",
        "ValidRaterCount",

        "ImageSizeBytes",
        "GenerationElapsedSeconds",
        "GenerationIterPerSecond",
        "GenerationVRAMUsage",
        "GenerationGPUUsage",
        "GenerationMemoryUsage",
        "GenerationCPUUsage",
    ]

    if "SubjectiveFileElapsedSeconds" in merged_df.columns:
        final_columns.append("SubjectiveFileElapsedSeconds")

    if "SubjectiveFileVRAMUsage" in merged_df.columns:
        final_columns.append("SubjectiveFileVRAMUsage")

    final_df = merged_df[final_columns].copy()

    summary_df = pd.DataFrame(
        [
            {"Item": "Subjective image-level rows", "Value": len(subjective_df)},
            {"Item": "Objective core rows", "Value": len(objective_df)},
            {"Item": "Merged rows", "Value": len(final_df)},
            {"Item": "Unmatched subjective images", "Value": len(unmatched_df)},
            {"Item": "Unique models", "Value": final_df["ModelShortName"].nunique()},
            {"Item": "Unique prompts", "Value": final_df["PromptID"].nunique()},
            {"Item": "Languages", "Value": ", ".join(sorted(final_df["Language"].astype(str).unique()))},
        ]
    )

    column_description_df = pd.DataFrame(
        [
            {"Column": "ImageName", "Description": "Image filename from the human-rated image-level dataset"},
            {"Column": "ObjectiveFileName", "Description": "Image filename from the objective core dataset"},
            {"Column": "ImageKey", "Description": "Normalized image filename used for merging"},
            {"Column": "ModelShortName", "Description": "Text-to-image model name"},
            {"Column": "PromptID", "Description": "Prompt identifier"},
            {"Column": "Language", "Description": "Prompt language"},
            {"Column": "Resolution", "Description": "Image generation resolution"},
            {"Column": "Steps", "Description": "Image generation inference steps"},
            {"Column": "SeedNo", "Description": "Random seed used for image generation"},
            {"Column": "OpenCLIPScore", "Description": "Automatic objective metric"},
            {"Column": "CNCLIPScore", "Description": "Automatic objective metric"},
            {"Column": "AestheticScore", "Description": "Automatic objective metric"},
            {"Column": "MUSIQScore", "Description": "Automatic objective metric"},
            {"Column": "ImageReward", "Description": "Automatic objective metric, renamed from ImageRewardScore"},
            {"Column": "HPSv21", "Description": "Automatic objective metric, renamed from HPSv2.1Score"},
            {"Column": "VQAScore", "Description": "Automatic objective metric"},
            {"Column": "MeanAdherence", "Description": "Human-rated mean prompt adherence score"},
            {"Column": "AdherenceSD", "Description": "Standard deviation of human-rated prompt adherence"},
            {"Column": "MeanClarity", "Description": "Human-rated mean image clarity score"},
            {"Column": "ClaritySD", "Description": "Standard deviation of human-rated image clarity"},
            {"Column": "MeanAesthetics", "Description": "Human-rated mean overall aesthetics score"},
            {"Column": "AestheticsSD", "Description": "Standard deviation of human-rated overall aesthetics"},
            {"Column": "CompositeScore", "Description": "Composite human rating score"},
            {"Column": "ValidRaterCount", "Description": "Number of valid raters for the image"},
            {"Column": "ImageSizeBytes", "Description": "Generated image file size in bytes"},
            {"Column": "GenerationElapsedSeconds", "Description": "Image generation elapsed time, renamed from objective ElapsedSeconds"},
            {"Column": "GenerationIterPerSecond", "Description": "Image generation iterations per second"},
            {"Column": "GenerationVRAMUsage", "Description": "VRAM usage during image generation"},
            {"Column": "GenerationGPUUsage", "Description": "GPU usage during image generation"},
            {"Column": "GenerationMemoryUsage", "Description": "System memory usage during image generation"},
            {"Column": "GenerationCPUUsage", "Description": "CPU usage during image generation"},
            {"Column": "SubjectiveFileElapsedSeconds", "Description": "ElapsedSeconds column from subjective image-level file, kept only for traceability"},
            {"Column": "SubjectiveFileVRAMUsage", "Description": "VRAMUsage column from subjective image-level file, kept only for traceability"},
        ]
    )

    print("Writing output Excel file...")

    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        final_df.to_excel(writer, sheet_name="analysis_120", index=False)
        summary_df.to_excel(writer, sheet_name="merge_summary", index=False)
        consistency_df.to_excel(writer, sheet_name="consistency_checks", index=False)
        column_description_df.to_excel(writer, sheet_name="column_description", index=False)

    print("Done.")
    print(f"Output file saved to: {output_path}")

    if len(final_df) != 120:
        print(
            f"Warning: merged row count is {len(final_df)}, not 120. "
            f"Please check whether the input data are complete."
        )
    else:
        print("Merged row count check passed: 120 rows.")


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"Error: {exc}", file=sys.stderr)
        sys.exit(1)
