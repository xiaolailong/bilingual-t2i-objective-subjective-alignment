#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  bash score_all_objective_metrics.sh <image_dir> [output_filename]

Examples:
  bash score_all_objective_metrics.sh ~/t2iPaper2/outputs/benchmark_cn
  bash score_all_objective_metrics.sh ~/t2iPaper2/outputs/benchmark_cn objective_scores.xlsx

Optional environment variables:
  DEVICE=cuda:0                    # default: auto in Python
  T2I_PAPER_HOME=~/t2iPaper2        # default: ~/t2iPaper2
  FAIL_FAST=1                      # stop on the first metric/image error, default: 1
  KEEP_TMP=1                       # keep intermediate JSONL files, default: 1
  OBJECTIVE_RESOURCE_SAMPLE_INTERVAL_SEC=0.5  # resource sampling interval
EOF
}

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMAGE_DIR="$(realpath "$1")"
OUTPUT_FILENAME="${2:-image_info_scores_all_metrics.xlsx}"
OUTPUT_STEM="${OUTPUT_FILENAME%.*}"
RESOURCE_OUTPUT_FILENAME="${OUTPUT_STEM}_metric_resources.xlsx"
DEVICE_ARG="${DEVICE:-}"
FAIL_FAST="${FAIL_FAST:-1}"
KEEP_TMP="${KEEP_TMP:-1}"

# -----------------------------------------------------------------------------
# Built-in replacement for env_objective_models.sh
# -----------------------------------------------------------------------------
export HF_HUB_OFFLINE=1
export TRANSFORMERS_OFFLINE=1

export T2I_PAPER_HOME="${T2I_PAPER_HOME:-$HOME/t2iPaper2}"
export T2I_MODELS_DIR="$T2I_PAPER_HOME/models"

# ImageReward and VQAScore use this Hugging Face cache directory.
export HF_HOME="$T2I_MODELS_DIR/hf_cache"
export HUGGINGFACE_HUB_CACHE="$HF_HOME/hub"
export TRANSFORMERS_CACHE="$HF_HOME/transformers"

# MUSIQ / pyiqa uses this torch cache directory.
export TORCH_HOME="$T2I_MODELS_DIR/torch_cache"

# HPS v2.1 local/cache root. Kept for compatibility with hpsv2 package variants.
export HPS_ROOT="$T2I_MODELS_DIR/hpsv2"

export TOKENIZERS_PARALLELISM=false

# -----------------------------------------------------------------------------
# Exact local paths based on the models/ tree supplied by the user.
# These are intentionally explicit, because several metric packages otherwise
# try to download by model name when their default cache layout is not matched.
# -----------------------------------------------------------------------------
export OPENCLIP_PRETRAINED="${OPENCLIP_PRETRAINED:-$T2I_MODELS_DIR/openclip/open_clip_pytorch_model.bin}"

export CNCLIP_ROOT="${CNCLIP_ROOT:-$T2I_MODELS_DIR/cnclip/ViT-L-14}"
export CNCLIP_CKPT="${CNCLIP_CKPT:-$T2I_MODELS_DIR/cnclip/ViT-L-14/clip_cn_vit-l-14.pt}"

export AESTHETIC_CLIP_MODEL="${AESTHETIC_CLIP_MODEL:-$T2I_MODELS_DIR/clip-vit-large-patch14}"
export AESTHETIC_HEAD_PATH="${AESTHETIC_HEAD_PATH:-$T2I_MODELS_DIR/aesthetic/sa_0_4_vit_l_14_linear.pth}"

export IMAGEREWARD_ROOT="${IMAGEREWARD_ROOT:-$T2I_MODELS_DIR/imagereward}"
export IMAGEREWARD_CKPT="${IMAGEREWARD_CKPT:-$T2I_MODELS_DIR/imagereward/ImageReward.pt}"
export IMAGEREWARD_MED_CONFIG="${IMAGEREWARD_MED_CONFIG:-$T2I_MODELS_DIR/imagereward/med_config.json}"

export HPSV21_CKPT="${HPSV21_CKPT:-$T2I_MODELS_DIR/hf_cache/hub/models--xswu--HPSv2/snapshots/697403c78157020a1ae59d23f111aa58ced35b0a/HPS_v2.1_compressed.pt}"

export VQASCORE_MODEL_ID="${VQASCORE_MODEL_ID:-Qwen/Qwen3-VL-2B-Instruct}"
export VQASCORE_LOCAL_MODEL="${VQASCORE_LOCAL_MODEL:-$T2I_MODELS_DIR/hf_cache/hub/models--Qwen--Qwen3-VL-2B-Instruct/snapshots/89644892e4d85e24eaac8bacfd4f463576704203}"
# t2v_metrics expects the package model alias, not the HF repo id or a local snapshot path.
# The local files are still resolved from HF_HOME/HUGGINGFACE_HUB_CACHE in offline mode.
export VQASCORE_MODEL_ALIAS="${VQASCORE_MODEL_ALIAS:-qwen3-vl-2b}"
export VQASCORE_MODEL="${VQASCORE_MODEL:-$VQASCORE_MODEL_ALIAS}"

mkdir -p "$HF_HOME/hub" "$HF_HOME/transformers" "$TORCH_HOME" "$HPS_ROOT"

# Expose the HF-cache HPS checkpoint under HPS_ROOT as well. Do not copy the
# 1.97GB file; use a symlink when available.
if [[ -f "$HPSV21_CKPT" && ! -e "$HPS_ROOT/HPS_v2.1_compressed.pt" ]]; then
  ln -s "$HPSV21_CKPT" "$HPS_ROOT/HPS_v2.1_compressed.pt" 2>/dev/null || true
fi

# -----------------------------------------------------------------------------
# Conda initialization
# -----------------------------------------------------------------------------
if [[ -f "$HOME/anaconda3/etc/profile.d/conda.sh" ]]; then
  # User's current layout in the project environment.
  # shellcheck source=/dev/null
  source "$HOME/anaconda3/etc/profile.d/conda.sh"
elif [[ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]]; then
  # shellcheck source=/dev/null
  source "$HOME/miniconda3/etc/profile.d/conda.sh"
elif command -v conda >/dev/null 2>&1; then
  eval "$(conda shell.bash hook)"
else
  echo "ERROR: conda was not found. Please install conda or adjust the conda initialization path in this script." >&2
  exit 1
fi

echo ""
echo "========== [preflight] local model paths =========="
conda activate "comfyui-py10"
python "$SCRIPT_DIR/objective_score.py" --print-local-model-paths

TMP_DIR="$IMAGE_DIR/.objective_scores_tmp_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$TMP_DIR"

PY_DEVICE_ARGS=()
if [[ -n "$DEVICE_ARG" ]]; then
  PY_DEVICE_ARGS+=(--device "$DEVICE_ARG")
fi

PY_FAIL_ARGS=()
if [[ "$FAIL_FAST" == "1" || "$FAIL_FAST" == "true" || "$FAIL_FAST" == "True" ]]; then
  PY_FAIL_ARGS+=(--fail-fast)
fi

run_metric() {
  local env_name="$1"
  local metric="$2"
  local output_jsonl="$3"

  echo ""
  echo "========== [$env_name] $metric =========="
  conda activate "$env_name"
  python "$SCRIPT_DIR/objective_score.py" \
    --image-dir "$IMAGE_DIR" \
    --metrics "$metric" \
    --output-jsonl "$TMP_DIR/$output_jsonl" \
    "${PY_DEVICE_ARGS[@]}" \
    "${PY_FAIL_ARGS[@]}"
}

run_metric "comfyui-py10" "OpenCLIPScore" "scores_openclip.jsonl"
run_metric "comfyui-py10" "CNCLIPScore" "scores_cnclip.jsonl"
run_metric "comfyui-py10" "AestheticScore" "scores_aesthetic.jsonl"
run_metric "comfyui-py10" "MUSIQScore" "scores_musiq.jsonl"
run_metric "imagereward-py10" "ImageRewardScore" "scores_imagereward.jsonl"
run_metric "hpsv2-py10" "HPSv2.1Score" "scores_hpsv21.jsonl"
run_metric "vqascore-py10" "VQAScore" "scores_vqascore.jsonl"

echo ""
echo "========== [merge] Excel =========="
conda activate "comfyui-py10"
python "$SCRIPT_DIR/extract_and_score_to_excel.py" "$IMAGE_DIR" \
  --score-jsonl "$TMP_DIR/scores_openclip.jsonl" \
  --score-jsonl "$TMP_DIR/scores_cnclip.jsonl" \
  --score-jsonl "$TMP_DIR/scores_aesthetic.jsonl" \
  --score-jsonl "$TMP_DIR/scores_musiq.jsonl" \
  --score-jsonl "$TMP_DIR/scores_imagereward.jsonl" \
  --score-jsonl "$TMP_DIR/scores_hpsv21.jsonl" \
  --score-jsonl "$TMP_DIR/scores_vqascore.jsonl" \
  --output-filename "$OUTPUT_FILENAME" \
  --resource-output-filename "$RESOURCE_OUTPUT_FILENAME"

if [[ "$KEEP_TMP" != "1" && "$KEEP_TMP" != "true" && "$KEEP_TMP" != "True" ]]; then
  rm -rf "$TMP_DIR"
else
  echo "Intermediate JSONL files kept at: $TMP_DIR"
fi

echo "Done. Excel output: $IMAGE_DIR/$OUTPUT_FILENAME"
echo "Metric resources Excel output: $IMAGE_DIR/$RESOURCE_OUTPUT_FILENAME"
