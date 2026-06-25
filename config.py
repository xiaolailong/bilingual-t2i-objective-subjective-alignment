#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from pathlib import Path
from typing import Any, Dict, List, Tuple


COMFY_BASE_URL = "http://127.0.0.1:8188"
COMFY_PORT = 8188
POLL_INTERVAL_SEC = 1.0
RESOURCE_SAMPLE_INTERVAL_SEC = 0.5
GENERATION_TIMEOUT_SEC = 1800

SCRIPT_DIR = Path(__file__).resolve().parent


SEEDS: List[int] = [
    765597514739706,
    270792713120448,
    729932648514692,
]

RESOLUTIONS: List[Tuple[int, int]] = [
    (512, 512),
    (768, 768),
    (1024, 1024),
]

MODEL_STEPS: Dict[str, List[int]] = {
    "zimage": [5, 9, 13],
    "qwen": [28, 40, 52],
    "sdxl": [1, 2, 4],
}

NEGATIVE_PROMPT = "low quality, blurry, artifacts"

MODEL_CONFIG: Dict[str, Dict[str, Any]] = {
    "zimage": {
        "json_path": str(SCRIPT_DIR / "conf" / "z_image_turbo.json"),
        "prompt_node_id": 6,
        "negative_prompt_node_id": 7,
        "latent_node_id": 13,
        "sampler_node_id": 3,
        "save_node_id": 9,
        "workflow_type": "zimage",
    },
    "qwen": {
        "json_path": str(SCRIPT_DIR / "conf" / "qwen_image_2512.json"),
        "prompt_node_id": 6,
        "negative_prompt_node_id": 7,
        "latent_node_id": 58,
        "sampler_node_id": 3,
        "save_node_id": 60,
        "workflow_type": "qwen",
    },
    "sdxl": {
        "json_path": str(SCRIPT_DIR / "conf" / "sdxl_turbo_txt2img.json"),
        "prompt_node_id": 6,
        "negative_prompt_node_id": 7,
        "latent_node_id": 5,
        "sampler_node_id": 13,       # SamplerCustom
        "steps_node_id": 22,         # SDTurboScheduler
        "save_node_id": 27,
        "workflow_type": "sdxl",
    },
}


UI_WIDGET_INDEX = {
    "clip_text_encode.text": 0,
    "latent.width": 0,
    "latent.height": 1,
    "latent.batch_size": 2,

    "ksampler.seed": 0,
    "ksampler.control_after_generate": 1,
    "ksampler.steps": 2,

    "samplercustom.add_noise": 0,
    "samplercustom.seed": 1,
    "samplercustom.control_after_generate": 2,

    "sdturbo_scheduler.steps": 0,

    "saveimage.filename_prefix": 0,
}


API_WIDGET_KEYS = {
    "CheckpointLoaderSimple": ["ckpt_name"],
    "EmptyLatentImage": ["width", "height", "batch_size"],
    "EmptySD3LatentImage": ["width", "height", "batch_size"],
    "CLIPTextEncode": ["text"],
    "KSampler": ["seed", "control_after_generate", "steps", "cfg", "sampler_name", "scheduler", "denoise"],
    "KSamplerSelect": ["sampler_name"],
    "SDTurboScheduler": ["steps", "denoise"],
    "SamplerCustom": ["add_noise", "noise_seed", "control_after_generate", "cfg"],
    "VAEDecode": [],
    "SaveImage": ["filename_prefix"],
    "VAELoader": ["vae_name"],
    "UNETLoader": ["unet_name", "weight_dtype"],
    "CLIPLoader": ["clip_name", "type", "device"],
    "UnetLoaderGGUF": ["unet_name"],
    "CLIPLoaderGGUF": ["clip_name", "type"],
    "ModelSamplingAuraFlow": ["shift"],
    "CFGNorm": ["strength"],
}
