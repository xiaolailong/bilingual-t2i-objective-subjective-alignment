# Code for bilingual T2I subjective-objective evaluation

This repository contains the scripts used for image generation, objective metric scoring, subjective-score aggregation, and RQ1-RQ4 statistical analyses for the manuscript:

**Alignment between Objective Metrics and Subjective Scores for AI-Generated Images under Bilingual Prompts**

The code is provided to support reproducibility of the reported analyses. Large model weights, generated images, and analysis data are not stored in this code repository; they are provided separately through the accompanying data archive.

## Repository structure

```text
code/
├── config.py
├── t2i.py
├── comfy_utils.py
├── core_subjective_to_image_level.py
├── merge_human_objective_120.py
├── objective_score.py
├── extract_and_score_to_excel.py
├── resource_monitor.py
├── score_all_objective_metrics.sh
├── conf/
│   ├── qwen_image_2512.json
│   ├── sdxl_turbo_txt2img.json
│   └── z_image_turbo.json
└── R/
    ├── reliability_analysis.R
    ├── rq1_human_objective_consistency.R
    ├── rq2_mismatch_analysis.R
    ├── rq3_objective_metric_behavior_analysis.R
    └── rq4_practical_applicability_analysis.R
```

## Main scripts

| Script | Purpose |
|---|---|
| `t2i.py` | Generates images from bilingual prompts using fixed ComfyUI workflows. |
| `config.py` | Stores generation settings, seeds, workflow paths, and ComfyUI node mappings. |
| `core_subjective_to_image_level.py` | Aggregates rating-level subjective scores to image-level means. |
| `merge_human_objective_120.py` | Merges Human120 subjective scores with objective scores for RQ1 and RQ2. |
| `objective_score.py` | Computes objective metrics for one image or an image directory. |
| `extract_and_score_to_excel.py` | Extracts image metadata, merges metric JSONL outputs, and writes Excel outputs. |
| `resource_monitor.py` | Records runtime and resource-use information during objective scoring. |
| `score_all_objective_metrics.sh` | Runs all seven objective metrics sequentially in their corresponding conda environments. |
| `R/reliability_analysis.R` | Estimates subjective-score reliability using mixed-effects variance components. |
| `R/rq1_human_objective_consistency.R` | Runs subjective-objective alignment analyses. |
| `R/rq2_mismatch_analysis.R` | Runs mismatch and residual-diagnostic analyses. |
| `R/rq3_objective_metric_behavior_analysis.R` | Runs objective-metric behavior and extension-stability analyses. |
| `R/rq4_practical_applicability_analysis.R` | Runs practical applicability analysis by integrating alignment, stability, cost, reliability, and implementation complexity. |

## Required data layout

The R scripts expect the following project layout:

```text
project_root/
├── code/
└── data/
    ├── prompts.xlsx
    ├── subjective_scores_human120.xlsx
    ├── subjective_scores_human120_image_level.xlsx
    ├── subjective_reliability_human120.xlsx
    ├── objective_scores_core360.xlsx
    ├── objective_scores_core360_resources.xlsx
    ├── objective_scores_extended1440.xlsx
    ├── objective_scores_extended1440_resources.xlsx
    ├── analysis_human_objective_120.xlsx
    ├── metric_deployment_complexity.csv
    ├── rq1_outputs/
    ├── rq2_outputs/
    ├── rq3_outputs/
    └── rq4_outputs/
```

Generated image archives, such as `coreImages.zip` and `extendedImages.zip`, should be stored in the data archive rather than in this GitHub repository.

## Environment notes

The objective metrics were run in separate conda environments because several metric packages have incompatible dependencies:

| Conda environment | Metrics |
|---|---|
| `comfyui-py10` | OpenCLIPScore, CNCLIPScore, AestheticScore, MUSIQScore |
| `imagereward-py10` | ImageReward |
| `hpsv2-py10` | HPS v2.1 |
| `vqascore-py10` | VQAScore |

The scoring pipeline is designed for local/offline model loading. By default, `score_all_objective_metrics.sh` sets cache and model paths under `~/t2iPaper2/models/`. Users who reproduce the scoring step should edit the local model paths in the shell script or set the corresponding environment variables.

## Running the pipeline

### 1. Image generation

```bash
python code/t2i.py data/prompts.xlsx data/coreImages human_rated_core
python code/t2i.py data/prompts.xlsx data/extendedImages objective_only_extended
```

The generation settings are defined in `config.py` and the workflow templates are stored under `code/conf/`.

### 2. Objective metric scoring

```bash
cd code
bash score_all_objective_metrics.sh ../data/coreImages objective_scores_core360.xlsx
bash score_all_objective_metrics.sh ../data/extendedImages objective_scores_extended1440.xlsx
```

The script runs seven metrics sequentially. Before each formal batch run, a warmup call is executed to reduce first-call model-loading and runtime-initialization effects; warmup results are not written to the formal scoring outputs.

### 3. Subjective-score aggregation and reliability analysis

```bash
python code/core_subjective_to_image_level.py data/subjective_scores_human120.xlsx data/coreImages -o data/subjective_scores_human120_image_level.xlsx
Rscript code/R/reliability_analysis.R
```

### 4. Merge subjective and objective scores for Human120

```bash
python code/merge_human_objective_120.py
```

This produces `data/analysis_human_objective_120.xlsx`, which is the main input for RQ1 and RQ2.

### 5. RQ analyses

Run from the project root:

```bash
Rscript code/R/rq1_human_objective_consistency.R
Rscript code/R/rq2_mismatch_analysis.R
Rscript code/R/rq3_objective_metric_behavior_analysis.R
Rscript code/R/rq4_practical_applicability_analysis.R
```

Each RQ script writes outputs to the corresponding `data/rq*_outputs/` directory and creates a README file explaining the generated results.

## Objective metrics

The analysis uses seven objective metrics:

- OpenCLIPScore
- CNCLIPScore
- AestheticScore
- MUSIQScore
- ImageReward
- HPS v2.1
- VQAScore

Text-based metrics use the original prompt text corresponding to each generated image. AestheticScore and MUSIQScore use only the image as input.

## Reproducibility notes

- Core360 contains 360 generated images: 20 core prompts × 3 generation sources × 2 prompt languages × 3 seeds.
- Human120 is a randomly sampled subset of Core360, with one image selected from each generation source-prompt-language condition for subjective scoring.
- Extended1440 contains 1440 generated images: 80 extended prompts × 3 generation sources × 2 prompt languages × 3 seeds.
- The R scripts use fixed seeds for repeated cross-validation where applicable.
- Objective scores and resource-use outputs in the data archive are sufficient to reproduce the reported statistical analyses without rerunning model inference.

## License and citation

Please add the final repository license selected by the authors before public release. A common choice is an open-source license such as MIT for code. When citing or reusing this code, please cite the corresponding manuscript and the archived data record.
