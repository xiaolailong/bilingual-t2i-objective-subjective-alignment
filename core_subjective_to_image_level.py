#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from __future__ import annotations

import argparse
import json
import math
import re
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import pandas as pd


IMAGE_CANONICAL_RE = re.compile(
    r'(c_[A-Za-z0-9\-]+_[A-Za-z0-9\-]+_(?:chinese|english)_[0-9]+-[0-9]+_step[0-9]+_[A-Za-z0-9\-]+\.png)$',
    re.IGNORECASE
)


def canonicalize_image_name(name: object) -> Optional[str]:
    """
    Info D.22.c_qwen_B13_english_1024-1024_step28_2.png
    Info c_qwen_B13_english_1024-1024_step28_2.png
    """
    if pd.isna(name):
        return None
    s = str(name).strip().replace("\\", "/")
    s = Path(s).name  # Remove path

    m = IMAGE_CANONICAL_RE.search(s)
    if m:
        return m.group(1)

    idx = s.find("c_")
    if idx >= 0:
        return s[idx:]

    return s if s else None


def normalize_columns(df: pd.DataFrame) -> pd.DataFrame:
    """
    Strip leading and trailing spaces from column names while preserving their semantics.
    """
    out = df.copy()
    out.columns = [str(c).strip() for c in out.columns]
    return out


REQUIRED_COLUMNS = [
    "respondent_id",
    "duration_seconds",
    "image_name",
    "prompt_id",
    "model",
    "language",
    "q_no",
    "score",
    "Content",
    "Style",
    "Complexity",
    "Context",
]


def read_all_rating_sheets(excel_path: Path) -> pd.DataFrame:
    xls = pd.ExcelFile(excel_path)
    all_frames: List[pd.DataFrame] = []

    for sheet_name in xls.sheet_names:
        if sheet_name == "teacher" or sheet_name.startswith("student_"):
            df = pd.read_excel(excel_path, sheet_name=sheet_name)
            df = normalize_columns(df)
            missing = [c for c in REQUIRED_COLUMNS if c not in df.columns]
            if missing:
                raise ValueError(f"sheet {sheet_name} missing required columns: {missing}")

            df["source_sheet"] = sheet_name
            all_frames.append(df)

    if not all_frames:
        raise ValueError("No teacher or student_* sheet was found.")

    merged = pd.concat(all_frames, ignore_index=True)

    merged["image_name"] = merged["image_name"].apply(canonicalize_image_name)
    merged["q_no"] = merged["q_no"].astype(str).str.strip().str.upper()
    merged["score"] = pd.to_numeric(merged["score"], errors="coerce")
    merged["duration_seconds"] = pd.to_numeric(merged["duration_seconds"], errors="coerce")

    merged = merged[merged["q_no"].isin(["Q1", "Q2", "Q3"])].copy()
    merged = merged[merged["score"].between(1, 5, inclusive="both")].copy()

    merged["respondent_id"] = merged["respondent_id"].astype(str).str.strip()

    key_cols = ["respondent_id", "image_name", "model", "prompt_id", "language", "Content", "Style", "Complexity", "Context"]
    merged = merged.dropna(subset=key_cols).copy()

    return merged


def build_valid_image_rater_table(ratings: pd.DataFrame) -> pd.DataFrame:
    """
    Info Q1/Q2/Q3, InfovalidInfo.
    Info-Info-Infoduplicate, InfoMean(Info, Info).
    """
    per_q = (
        ratings.groupby(
            [
                "image_name",
                "respondent_id",
                "model",
                "prompt_id",
                "language",
                "Content",
                "Style",
                "Complexity",
                "Context",
                "q_no",
            ],
            as_index=False
        )["score"].mean()
    )

    wide = per_q.pivot_table(
        index=[
            "image_name",
            "respondent_id",
            "model",
            "prompt_id",
            "language",
            "Content",
            "Style",
            "Complexity",
            "Context",
        ],
        columns="q_no",
        values="score",
        aggfunc="mean"
    ).reset_index()

    wide.columns.name = None

    for q in ["Q1", "Q2", "Q3"]:
        if q not in wide.columns:
            wide[q] = pd.NA

    valid = wide.dropna(subset=["Q1", "Q2", "Q3"]).copy()
    return valid


NUMBER_RE = re.compile(r'[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?')


def parse_first_number(text: str) -> Optional[float]:
    m = NUMBER_RE.search(text)
    if not m:
        return None
    try:
        return float(m.group())
    except Exception:
        return None


def safe_get(d: object, path: List[str]) -> Optional[object]:
    cur = d
    for key in path:
        if not isinstance(cur, dict) or key not in cur:
            return None
        cur = cur[key]
    return cur


def to_float(value: object) -> Optional[float]:
    if value is None:
        return None
    try:
        return float(value)
    except Exception:
        return None


def parse_txt_metadata(txt_path: Path) -> Dict[str, Optional[float]]:
    """
    Parse txt as JSON first.

    RationalewhenInfo:
    - ElapsedSeconds:monitor.elapsed_seconds
    - VRAMUsage:monitor.gpu.memory_used_mb.max

    If JSON parsing fails, fall back to the old text-matching logic.
    """
    result = {
        "elapsed_seconds": None,
        "vram_usage": None,
    }

    if not txt_path or not txt_path.exists():
        return result

    try:
        content = txt_path.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        return result

    try:
        obj = json.loads(content)
        result["elapsed_seconds"] = to_float(safe_get(obj, ["monitor", "elapsed_seconds"]))

        result["vram_usage"] = (
            to_float(safe_get(obj, ["monitor", "gpu", "memory_used_mb", "max"]))
            if safe_get(obj, ["monitor", "gpu", "memory_used_mb", "max"]) is not None
            else None
        )
        if result["vram_usage"] is None:
            result["vram_usage"] = to_float(safe_get(obj, ["monitor", "gpu", "memory_used_mb", "avg"]))
        if result["vram_usage"] is None:
            result["vram_usage"] = to_float(safe_get(obj, ["monitor", "gpu", "memory_used_mb", "min"]))

        return result
    except Exception:
        pass

    lines = content.splitlines()

    elapsed_aliases = [
        "elapsed_seconds", "elapsed", "duration_seconds", "time_seconds", "ElapsedSeconds"
    ]
    vram_aliases = [
        "VRAMUsage", "vram", "vram_mb", "gpu_memory", "gpu_memory_mb",
        "peak_vram_mb", "max_vram_mb", "memory_allocated_mb"
    ]

    def try_match_from_lines(aliases: List[str]) -> Optional[float]:
        for line in lines:
            lower_line = line.lower()
            for key in aliases:
                if key.lower() in lower_line:
                    value = parse_first_number(line)
                    if value is not None:
                        return value
        return None

    result["elapsed_seconds"] = try_match_from_lines(elapsed_aliases)
    result["vram_usage"] = try_match_from_lines(vram_aliases)

    if result["elapsed_seconds"] is None:
        for key in elapsed_aliases:
            m = re.search(rf'{re.escape(key)}\s*[:=:]?\s*([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)', content, flags=re.I)
            if m:
                result["elapsed_seconds"] = float(m.group(1))
                break

    if result["vram_usage"] is None:
        for key in vram_aliases:
            m = re.search(rf'{re.escape(key)}\s*[:=:]?\s*([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)', content, flags=re.I)
            if m:
                result["vram_usage"] = float(m.group(1))
                break

    return result


def build_recursive_txt_index(meta_dir: Path) -> Dict[str, Path]:
    """
    Recursively traverse the directory and subdirectories to build a {txtFileName: path} index.
    If duplicate txt basenames appear, keep the first one found.
    """
    index: Dict[str, Path] = {}
    for p in meta_dir.rglob("*.txt"):
        if p.name not in index:
            index[p.name] = p
    return index


def load_all_txt_metadata(meta_dir: Path, image_names: Iterable[str]) -> pd.DataFrame:
    txt_index = build_recursive_txt_index(meta_dir)

    rows = []
    for image_name in sorted(set(image_names)):
        txt_name = Path(image_name).with_suffix(".txt").name
        txt_path = txt_index.get(txt_name)
        meta = parse_txt_metadata(txt_path) if txt_path else {"elapsed_seconds": None, "vram_usage": None}
        rows.append({
            "image_name": image_name,
            "ElapsedSeconds": meta["elapsed_seconds"],
            "VRAMUsage": meta["vram_usage"],
        })
    return pd.DataFrame(rows)


def aggregate_to_image_level(valid_image_rater: pd.DataFrame) -> pd.DataFrame:
    grouped = (
        valid_image_rater.groupby(
            [
                "image_name",
                "model",
                "prompt_id",
                "language",
                "Content",
                "Style",
                "Complexity",
                "Context",
            ],
            as_index=False
        )
        .agg(
            MeanAdherence=("Q1", "mean"),
            AdherenceSD=("Q1", "std"),
            MeanClarity=("Q2", "mean"),
            ClaritySD=("Q2", "std"),
            MeanAesthetics=("Q3", "mean"),
            AestheticsSD=("Q3", "std"),
            ValidRaterCount=("respondent_id", "nunique"),
        )
    )

    grouped["CompositeScore"] = (
        grouped["MeanAdherence"] + grouped["MeanClarity"] + grouped["MeanAesthetics"]
    ) / 3.0

    grouped = grouped[
        [
            "image_name",
            "model",
            "prompt_id",
            "language",
            "Content",
            "Style",
            "Complexity",
            "Context",
            "MeanAdherence",
            "AdherenceSD",
            "MeanClarity",
            "ClaritySD",
            "MeanAesthetics",
            "AestheticsSD",
            "CompositeScore",
            "ValidRaterCount",
        ]
    ].copy()

    grouped = grouped.rename(columns={
        "image_name": "ImageName",
        "model": "ModelShortName",
        "prompt_id": "PromptID",
        "language": "Language",
        "Content": "ContentAttribute",
        "Style": "StyleAttribute",
        "Complexity": "ComplexityAttribute",
        "Context": "ContextAttribute",
    })

    return grouped


def build_n_raters_summary(image_level_df: pd.DataFrame) -> pd.DataFrame:
    s = pd.to_numeric(image_level_df["ValidRaterCount"], errors="coerce")
    summary = pd.DataFrame(
        {
            "Metric": ["min", "max", "mean", "std"],
            "ValidRaterCount": [
                s.min(),
                s.max(),
                s.mean(),
                s.std(ddof=1),
            ],
        }
    )
    return summary


def save_output(image_level_df: pd.DataFrame, summary_df: pd.DataFrame, out_path: Path) -> None:
    with pd.ExcelWriter(out_path, engine="openpyxl") as writer:
        image_level_df.to_excel(writer, sheet_name="image_level", index=False)
        summary_df.to_excel(writer, sheet_name="n_raters_summary", index=False)


def main():
    parser = argparse.ArgumentParser(description="Aggregate rating-level data to image-level data")
    parser.add_argument("input_excel", type=str, help="rating-level Excel file path")
    parser.add_argument("meta_dir", type=str, help="directory containing png/txt files")
    parser.add_argument("-o", "--output", type=str, default=None, help="output Excel path")
    args = parser.parse_args()

    input_excel = Path(args.input_excel)
    meta_dir = Path(args.meta_dir)

    if not input_excel.exists():
        raise FileNotFoundError(f"Input Excel does not exist: {input_excel}")
    if not meta_dir.exists():
        raise FileNotFoundError(f"Metadata directory does not exist: {meta_dir}")

    if args.output:
        out_path = Path(args.output)
    else:
        out_path = input_excel.with_name(input_excel.stem + "_image_level.xlsx")

    ratings = read_all_rating_sheets(input_excel)

    valid_image_rater = build_valid_image_rater_table(ratings)

    if valid_image_rater.empty:
        raise ValueError("No image-rater record contains Q1/Q2/Q3 together; aggregation is impossible.")

    image_level = aggregate_to_image_level(valid_image_rater)

    meta_df = load_all_txt_metadata(meta_dir, image_level["ImageName"].tolist())
    image_level = image_level.merge(
        meta_df,
        left_on="ImageName",
        right_on="image_name",
        how="left"
    ).drop(columns=["image_name"])

    final_cols = [
        "ImageName",
        "ModelShortName",
        "PromptID",
        "Language",
        "ContentAttribute",
        "StyleAttribute",
        "ComplexityAttribute",
        "ContextAttribute",
        "MeanAdherence",
        "AdherenceSD",
        "MeanClarity",
        "ClaritySD",
        "MeanAesthetics",
        "AestheticsSD",
        "CompositeScore",
        "ValidRaterCount",
        "ElapsedSeconds",
        "VRAMUsage",
    ]
    image_level = image_level[final_cols].copy()

    summary_df = build_n_raters_summary(image_level)

    image_level = image_level.sort_values(
        by=["ModelShortName", "PromptID", "Language", "ImageName"],
        kind="stable"
    ).reset_index(drop=True)

    save_output(image_level, summary_df, out_path)

    print(f"Info:{out_path}")
    print(f"Info:{len(image_level)}")
    print("ValidRaterCountInfo:")
    print(summary_df.to_string(index=False))


if __name__ == "__main__":
    main()
