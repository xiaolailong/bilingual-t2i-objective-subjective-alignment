#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Objective metric scoring utilities for the second T2I paper project.

This file is designed to be called inside different conda environments.
It keeps imports lazy, so environments only need the packages required by
metrics that are actually requested in that run.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
import warnings
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, Literal, Optional, Sequence, Tuple, Union

from PIL import Image

Language = Literal["chinese", "english", "zh", "en"]

IMAGE_SUFFIXES = {".png", ".jpg", ".jpeg", ".webp", ".bmp"}
FILENAME_PATTERN = re.compile(
    r"^c_"
    r"(?P<model>[^_]+)_"
    r"(?P<prompt_no>[^_]+)_"
    r"(?P<language>chinese|english)_"
    r"(?P<width>\d+)-(?P<height>\d+)_"
    r"step(?P<step>\d+)_"
    r"(?P<seed_no>\d+)"
    r"\.(?P<ext>png|jpg|jpeg|webp|bmp)$",
    re.IGNORECASE,
)

# -----------------------------------------------------------------------------
# Environment and local model paths
# -----------------------------------------------------------------------------

BASE_DIR = Path(__file__).resolve().parent


def _detect_project_dir() -> Path:
    env_home = os.environ.get("T2I_PAPER_HOME")
    if env_home:
        return Path(env_home).expanduser().resolve()

    # Compatible with either:
    #   ~/t2iPaper2/objective_score.py
    #   ~/t2iPaper2/code/objective_score.py
    for candidate in [BASE_DIR, BASE_DIR.parent, BASE_DIR.parent.parent]:
        if (candidate / "models").exists():
            return candidate.resolve()

    # Fallback to the current historical layout: script under code/, models/ above.
    return BASE_DIR.parent.resolve()


PROJECT_DIR = _detect_project_dir()
MODELS_DIR = PROJECT_DIR / "models"
HF_CACHE_DIR = MODELS_DIR / "hf_cache"
TORCH_CACHE_DIR = MODELS_DIR / "torch_cache"
HPS_ROOT_DIR = MODELS_DIR / "hpsv2"


def setup_objective_environment() -> None:
    """Set all objective model/cache environment variables in Python.

    This mirrors env_objective_models.sh, so the user does not need to run
    `source env_objective_models.sh` manually before launching the pipeline.
    The shell entry script also exports the same variables for child processes.
    """

    os.environ.setdefault("HF_HUB_OFFLINE", "1")
    os.environ.setdefault("TRANSFORMERS_OFFLINE", "1")

    os.environ.setdefault("T2I_PAPER_HOME", str(PROJECT_DIR))
    os.environ.setdefault("T2I_MODELS_DIR", str(MODELS_DIR))

    os.environ.setdefault("HF_HOME", str(HF_CACHE_DIR))
    os.environ.setdefault("HUGGINGFACE_HUB_CACHE", str(HF_CACHE_DIR / "hub"))
    os.environ.setdefault("TRANSFORMERS_CACHE", str(HF_CACHE_DIR / "transformers"))

    os.environ.setdefault("TORCH_HOME", str(TORCH_CACHE_DIR))
    os.environ.setdefault("HPS_ROOT", str(HPS_ROOT_DIR))
    os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")

    HF_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    (HF_CACHE_DIR / "hub").mkdir(parents=True, exist_ok=True)
    (HF_CACHE_DIR / "transformers").mkdir(parents=True, exist_ok=True)
    TORCH_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    HPS_ROOT_DIR.mkdir(parents=True, exist_ok=True)


setup_objective_environment()

LOCAL_FILES_ONLY = os.environ.get("OBJ_LOCAL_FILES_ONLY", "1") not in {"0", "false", "False"}

DEFAULT_OPENCLIP_MODEL_NAME = os.environ.get("OPENCLIP_MODEL_NAME", "ViT-H-14")
DEFAULT_OPENCLIP_PRETRAINED = Path(
    os.environ.get(
        "OPENCLIP_PRETRAINED",
        str(MODELS_DIR / "openclip" / "open_clip_pytorch_model.bin"),
    )
)

DEFAULT_CNCLIP_MODEL_NAME = os.environ.get("CNCLIP_MODEL_NAME", "ViT-L-14")
DEFAULT_CNCLIP_ROOT = Path(os.environ.get("CNCLIP_ROOT", str(MODELS_DIR / "cnclip")))
DEFAULT_CNCLIP_CKPT = os.environ.get("CNCLIP_CKPT")
DEFAULT_CNCLIP_CKPT_FILENAME = os.environ.get("CNCLIP_CKPT_FILENAME", "clip_cn_vit-l-14.pt")

DEFAULT_AESTHETIC_CLIP_MODEL = Path(
    os.environ.get("AESTHETIC_CLIP_MODEL", str(MODELS_DIR / "clip-vit-large-patch14"))
)
DEFAULT_AESTHETIC_HEAD_PATH = Path(
    os.environ.get("AESTHETIC_HEAD_PATH", str(MODELS_DIR / "aesthetic" / "sa_0_4_vit_l_14_linear.pth"))
)

DEFAULT_MUSIQ_METRIC = os.environ.get("MUSIQ_METRIC", "musiq")

DEFAULT_IMAGEREWARD_ROOT = Path(os.environ.get("IMAGEREWARD_ROOT", str(MODELS_DIR / "imagereward")))
DEFAULT_IMAGEREWARD_CKPT = Path(
    os.environ.get("IMAGEREWARD_CKPT", str(DEFAULT_IMAGEREWARD_ROOT / "ImageReward.pt"))
)
DEFAULT_IMAGEREWARD_MED_CONFIG = Path(
    os.environ.get("IMAGEREWARD_MED_CONFIG", str(DEFAULT_IMAGEREWARD_ROOT / "med_config.json"))
)
DEFAULT_HPS_VERSION = os.environ.get("HPS_VERSION", "v2.1")
DEFAULT_HPSV21_CKPT = Path(
    os.environ.get(
        "HPSV21_CKPT",
        str(
            HF_CACHE_DIR
            / "hub"
            / "models--xswu--HPSv2"
            / "snapshots"
            / "697403c78157020a1ae59d23f111aa58ced35b0a"
            / "HPS_v2.1_compressed.pt"
        ),
    )
)
DEFAULT_VQASCORE_MODEL_ID = os.environ.get("VQASCORE_MODEL_ID", "Qwen/Qwen3-VL-2B-Instruct")
# t2v_metrics VQAScore expects its own short model alias, not a Hugging Face
# repo id or a local snapshot path. The local snapshot below is still used via
# HF_HOME/HUGGINGFACE_HUB_CACHE/TRANSFORMERS_CACHE in offline mode.
DEFAULT_VQASCORE_MODEL_ALIAS = os.environ.get("VQASCORE_MODEL_ALIAS", "qwen3-vl-2b")
DEFAULT_VQASCORE_LOCAL_MODEL = Path(
    os.environ.get(
        "VQASCORE_LOCAL_MODEL",
        str(
            HF_CACHE_DIR
            / "hub"
            / "models--Qwen--Qwen3-VL-2B-Instruct"
            / "snapshots"
            / "89644892e4d85e24eaac8bacfd4f463576704203"
        ),
    )
)
DEFAULT_VQASCORE_MODEL = os.environ.get("VQASCORE_MODEL", DEFAULT_VQASCORE_MODEL_ALIAS)

# -----------------------------------------------------------------------------
# Generic helpers
# -----------------------------------------------------------------------------


def _smart_cast(value: Any) -> Any:
    if not isinstance(value, str):
        return value
    text = value.strip()
    if text == "":
        return ""
    lower = text.lower()
    if lower in {"none", "null"}:
        return None
    if lower in {"true", "false"}:
        return lower == "true"
    if re.fullmatch(r"[+-]?\d+", text):
        try:
            return int(text)
        except Exception:
            pass
    if re.fullmatch(r"[+-]?\d+(\.\d+)?", text):
        try:
            return float(text)
        except Exception:
            pass
    return text


def _parse_key_value_text(text: str) -> Dict[str, Any]:
    data: Dict[str, Any] = {}
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        m = re.match(r"^([A-Za-z0-9_\-\u4e00-\u9fa5]+)\s*:\s*(.+)$", line)
        if not m:
            m = re.match(r"^([A-Za-z0-9_\-\u4e00-\u9fa5]+)\s*=\s*(.+)$", line)
        if m:
            data[m.group(1).strip()] = _smart_cast(m.group(2).strip())
    return data


def _read_sidecar_txt(txt_path: Path) -> Dict[str, Any]:
    text = txt_path.read_text(encoding="utf-8").strip()
    if not text:
        return {}
    try:
        obj = json.loads(text)
        if isinstance(obj, dict):
            return obj
    except Exception:
        pass
    return _parse_key_value_text(text)


def _pick_first_value(data: Dict[str, Any], keys: Sequence[str], default: Any = None) -> Any:
    for key in keys:
        if key in data:
            return data[key]
    lowered = {str(k).lower(): v for k, v in data.items()}
    for key in keys:
        lk = key.lower()
        if lk in lowered:
            return lowered[lk]
    return default


def _normalize_language(language: str) -> str:
    lang = language.strip().lower()
    if lang in {"chinese", "zh", "cn"}:
        return "chinese"
    if lang in {"english", "en"}:
        return "english"
    raise ValueError(f"Unsupported language: {language!r}. Please use chinese/english or zh/en.")


def _load_image_rgb(image_path: str | Path) -> Image.Image:
    return Image.open(image_path).convert("RGB")


def _get_torch():
    import torch

    return torch


def _get_device(device: Optional[str] = None):
    torch = _get_torch()
    if device:
        return torch.device(device)
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


def _l2_normalize(x: Any) -> Any:
    return x / x.norm(dim=-1, keepdim=True).clamp(min=1e-12)


def _require_exists(path: Path, what: str) -> str:
    path = path.expanduser().resolve()
    if not path.exists():
        raise FileNotFoundError(
            f"{what} not found: {path}\n"
            f"PROJECT_DIR={PROJECT_DIR}\n"
            f"MODELS_DIR={MODELS_DIR}\n"
            f"Please check the local models directory or override the related path with an environment variable."
        )
    return str(path)


def _to_scalar(value: Any) -> float:
    """Convert tensors, numpy values, nested lists, or scalar-like objects to float."""
    current = value
    while isinstance(current, (list, tuple)):
        if len(current) == 0:
            raise ValueError("Cannot convert an empty list/tuple to a scalar score.")
        current = current[0]

    if isinstance(current, dict):
        for key in ["score", "scores", "value", "result"]:
            if key in current:
                return _to_scalar(current[key])
        raise ValueError(f"Cannot convert dict score to scalar. Keys={list(current.keys())}")

    if hasattr(current, "detach"):
        current = current.detach()
    if hasattr(current, "cpu"):
        current = current.cpu()
    if hasattr(current, "numpy"):
        try:
            current = current.numpy()
        except Exception:
            pass
    if hasattr(current, "item"):
        try:
            return float(current.item())
        except Exception:
            pass
    try:
        return float(current)
    except Exception as exc:
        raise ValueError(f"Cannot convert score value to float: {type(value)!r}, {value!r}") from exc


# -----------------------------------------------------------------------------
# Metric implementations. All heavy imports are lazy.
# -----------------------------------------------------------------------------

_OPENCLIP_MODEL = None
_OPENCLIP_PREPROCESS = None
_OPENCLIP_TOKENIZER = None

_CNCLIP_MODEL = None
_CNCLIP_PREPROCESS = None
_CNCLIP_MODULE = None

_AESTHETIC_CLIP_MODEL_OBJ = None
_AESTHETIC_CLIP_PROCESSOR = None
_AESTHETIC_HEAD = None

_MUSIQ_METRIC = None
_IMAGEREWARD_MODEL = None
_VQASCORE_MODEL_OBJ = None


def _get_openclip(device: Any):
    global _OPENCLIP_MODEL, _OPENCLIP_PREPROCESS, _OPENCLIP_TOKENIZER
    if _OPENCLIP_MODEL is None or _OPENCLIP_PREPROCESS is None or _OPENCLIP_TOKENIZER is None:
        import open_clip

        pretrained = _require_exists(DEFAULT_OPENCLIP_PRETRAINED, "OpenCLIP weights")
        _OPENCLIP_MODEL, _, _OPENCLIP_PREPROCESS = open_clip.create_model_and_transforms(
            model_name=DEFAULT_OPENCLIP_MODEL_NAME,
            pretrained=pretrained,
            device=device,
        )
        _OPENCLIP_MODEL.eval()
        _OPENCLIP_TOKENIZER = open_clip.get_tokenizer(DEFAULT_OPENCLIP_MODEL_NAME)
    _OPENCLIP_MODEL.to(device)
    return _OPENCLIP_MODEL, _OPENCLIP_PREPROCESS, _OPENCLIP_TOKENIZER


def _compute_openclip(image_path: str | Path, prompt: str, device: Any) -> float:
    torch = _get_torch()
    image = _load_image_rgb(image_path)
    model, preprocess, tokenizer = _get_openclip(device)
    image_tensor = preprocess(image).unsqueeze(0).to(device)
    text_tensor = tokenizer([prompt]).to(device)
    with torch.no_grad():
        image_features = _l2_normalize(model.encode_image(image_tensor))
        text_features = _l2_normalize(model.encode_text(text_tensor))
        cosine = torch.sum(image_features * text_features, dim=-1).item()
    return float(max(100.0 * cosine, 0.0))


def _resolve_cnclip_checkpoint() -> Path:
    """Find the local CNCLIP checkpoint without contacting Hugging Face.

    cn_clip.load_from_name() expects clip_cn_vit-l-14.pt directly under
    download_root. The user's current layout is usually:
        models/cnclip/ViT-L-14/clip_cn_vit-l-14.pt
    so we resolve the exact checkpoint path first and pass that path to cn_clip
    whenever the package version supports it.
    """

    candidates: List[Path] = []
    if DEFAULT_CNCLIP_CKPT:
        candidates.append(Path(DEFAULT_CNCLIP_CKPT).expanduser())

    root = DEFAULT_CNCLIP_ROOT.expanduser()
    candidates.extend(
        [
            root / DEFAULT_CNCLIP_CKPT_FILENAME,
            root / DEFAULT_CNCLIP_MODEL_NAME / DEFAULT_CNCLIP_CKPT_FILENAME,
            MODELS_DIR / "cnclip" / DEFAULT_CNCLIP_MODEL_NAME / DEFAULT_CNCLIP_CKPT_FILENAME,
            MODELS_DIR / "cnclip" / DEFAULT_CNCLIP_CKPT_FILENAME,
        ]
    )

    seen = set()
    unique_candidates: List[Path] = []
    for candidate in candidates:
        resolved_key = str(candidate.expanduser())
        if resolved_key not in seen:
            unique_candidates.append(candidate.expanduser())
            seen.add(resolved_key)

    for candidate in unique_candidates:
        if candidate.exists():
            return candidate.resolve()

    checked = "\n".join(f"  - {p}" for p in unique_candidates)
    raise FileNotFoundError(
        "CNCLIP checkpoint was not found locally. Checked:\n"
        f"{checked}\n"
        "Expected file name: clip_cn_vit-l-14.pt. "
        "For your current layout, CNCLIP_CKPT should usually be:\n"
        f"  {MODELS_DIR / 'cnclip' / 'ViT-L-14' / DEFAULT_CNCLIP_CKPT_FILENAME}"
    )


def _get_cnclip(device: Any):
    global _CNCLIP_MODEL, _CNCLIP_PREPROCESS, _CNCLIP_MODULE
    if _CNCLIP_MODEL is None or _CNCLIP_PREPROCESS is None or _CNCLIP_MODULE is None:
        try:
            import cn_clip.clip as cn_clip
        except Exception:
            from cn_clip import clip as cn_clip  # type: ignore

        ckpt_path = _resolve_cnclip_checkpoint()

        # Preferred path: many cn_clip versions accept a local .pt path as the
        # first argument. This bypasses hf_hub_download completely.
        try:
            _CNCLIP_MODEL, _CNCLIP_PREPROCESS = cn_clip.load_from_name(
                str(ckpt_path),
                device=device,
                download_root=str(ckpt_path.parent),
            )
        except Exception as path_error:
            # Compatibility fallback: some cn_clip versions only accept a model
            # name, but then they expect the checkpoint directly under
            # download_root. Passing ckpt_path.parent works for layouts like:
            # models/cnclip/ViT-L-14/clip_cn_vit-l-14.pt
            try:
                _CNCLIP_MODEL, _CNCLIP_PREPROCESS = cn_clip.load_from_name(
                    DEFAULT_CNCLIP_MODEL_NAME,
                    device=device,
                    download_root=str(ckpt_path.parent),
                )
            except Exception as name_error:
                raise RuntimeError(
                    "Failed to load local CNCLIP checkpoint.\n"
                    f"Resolved checkpoint: {ckpt_path}\n"
                    f"First error when loading by local path: {path_error}\n"
                    f"Second error when loading by model name and local root: {name_error}"
                ) from name_error

        _CNCLIP_MODEL.eval()
        _CNCLIP_MODULE = cn_clip
    _CNCLIP_MODEL.to(device)
    return _CNCLIP_MODEL, _CNCLIP_PREPROCESS, _CNCLIP_MODULE


def _compute_cnclip(image_path: str | Path, prompt: str, device: Any) -> float:
    torch = _get_torch()
    image = _load_image_rgb(image_path)
    model, preprocess, cn_clip = _get_cnclip(device)
    image_tensor = preprocess(image).unsqueeze(0).to(device)
    text_tensor = cn_clip.tokenize([prompt]).to(device)
    with torch.no_grad():
        image_features = _l2_normalize(model.encode_image(image_tensor))
        text_features = _l2_normalize(model.encode_text(text_tensor))
        cosine = torch.sum(image_features * text_features, dim=-1).item()
    return float(max(100.0 * cosine, 0.0))


def _get_aesthetic_models(device: Any):
    global _AESTHETIC_CLIP_MODEL_OBJ, _AESTHETIC_CLIP_PROCESSOR, _AESTHETIC_HEAD
    if _AESTHETIC_CLIP_MODEL_OBJ is None or _AESTHETIC_CLIP_PROCESSOR is None:
        from transformers import CLIPModel, CLIPProcessor

        model_path = _require_exists(DEFAULT_AESTHETIC_CLIP_MODEL, "Aesthetic CLIP model directory")
        _AESTHETIC_CLIP_MODEL_OBJ = CLIPModel.from_pretrained(
            model_path,
            local_files_only=LOCAL_FILES_ONLY,
        )
        _AESTHETIC_CLIP_PROCESSOR = CLIPProcessor.from_pretrained(
            model_path,
            local_files_only=LOCAL_FILES_ONLY,
        )
        _AESTHETIC_CLIP_MODEL_OBJ.eval()

    if _AESTHETIC_HEAD is None:
        torch = _get_torch()
        import torch.nn as nn

        head_path = _require_exists(DEFAULT_AESTHETIC_HEAD_PATH, "Aesthetic linear head")
        head = nn.Linear(768, 1)
        state_dict = torch.load(head_path, map_location="cpu")
        head.load_state_dict(state_dict)
        head.eval()
        _AESTHETIC_HEAD = head

    _AESTHETIC_CLIP_MODEL_OBJ.to(device)
    _AESTHETIC_HEAD.to(device)
    return _AESTHETIC_CLIP_MODEL_OBJ, _AESTHETIC_CLIP_PROCESSOR, _AESTHETIC_HEAD


def _compute_aesthetic(image_path: str | Path, _prompt: str, device: Any) -> float:
    torch = _get_torch()
    image = _load_image_rgb(image_path)
    model, processor, head = _get_aesthetic_models(device)
    with torch.no_grad():
        inputs = processor(images=image, return_tensors="pt")
        inputs = {k: v.to(device) for k, v in inputs.items()}
        vision_outputs = model.vision_model(
            pixel_values=inputs["pixel_values"],
            return_dict=True,
        )
        image_features = model.visual_projection(vision_outputs.pooler_output)
        image_features = _l2_normalize(image_features)
        return float(head(image_features).squeeze().item())


def _get_musiq(device: Any):
    global _MUSIQ_METRIC
    if _MUSIQ_METRIC is None:
        import pyiqa

        _MUSIQ_METRIC = pyiqa.create_metric(DEFAULT_MUSIQ_METRIC, device=device)
    return _MUSIQ_METRIC


def _compute_musiq(image_path: str | Path, _prompt: str, device: Any) -> float:
    torch = _get_torch()
    metric = _get_musiq(device)
    with torch.no_grad():
        score = metric(str(image_path))
    return _to_scalar(score)


def _get_imagereward(device: Any):
    global _IMAGEREWARD_MODEL
    if _IMAGEREWARD_MODEL is None:
        import ImageReward as RM

        # Important: do NOT call RM.load("ImageReward-v1.0") here in offline mode.
        # That name path triggers hf_hub_download inside ImageReward. The user's
        # project layout already has a direct local checkpoint:
        #   models/imagereward/ImageReward.pt
        # and its config:
        #   models/imagereward/med_config.json
        ckpt_path = _require_exists(DEFAULT_IMAGEREWARD_CKPT, "ImageReward checkpoint")
        med_config_path = _require_exists(DEFAULT_IMAGEREWARD_MED_CONFIG, "ImageReward med_config")

        last_error: Optional[BaseException] = None
        for kwargs in [
            {"device": str(device), "med_config": med_config_path},
            {"med_config": med_config_path},
            {"device": str(device)},
            {},
        ]:
            try:
                _IMAGEREWARD_MODEL = RM.load(ckpt_path, **kwargs)
                break
            except TypeError as exc:
                last_error = exc
                continue

        if _IMAGEREWARD_MODEL is None:
            assert last_error is not None
            raise last_error

        if hasattr(_IMAGEREWARD_MODEL, "eval"):
            _IMAGEREWARD_MODEL.eval()
    return _IMAGEREWARD_MODEL


def _compute_imagereward(image_path: str | Path, prompt: str, device: Any) -> float:
    model = _get_imagereward(device)
    score = model.score(prompt, str(image_path))
    return _to_scalar(score)


def _prepare_hps_checkpoint() -> None:
    """Expose the locally cached HPS v2.1 checkpoint to the hpsv2 package.

    The user's directory tree stores the checkpoint in the Hugging Face cache:
      models/hf_cache/hub/models--xswu--HPSv2/snapshots/.../HPS_v2.1_compressed.pt
    The official hpsv2 package also honors HPS_ROOT as a cache root, so we
    create a lightweight symlink under models/hpsv2/ when possible.
    """

    ckpt = DEFAULT_HPSV21_CKPT.expanduser()
    if not ckpt.exists():
        # Some setups may rely on the hpsv2 package's own cache. Let hpsv2 report
        # its own error instead of failing earlier.
        return

    os.environ.setdefault("HPS_ROOT", str(HPS_ROOT_DIR))
    HPS_ROOT_DIR.mkdir(parents=True, exist_ok=True)
    target = HPS_ROOT_DIR / "HPS_v2.1_compressed.pt"
    if target.exists():
        return
    try:
        target.symlink_to(ckpt)
    except Exception:
        # If symlink is not allowed, do not copy a ~2GB checkpoint automatically.
        # hpsv2 may still be able to use the HF cache directly.
        pass


def _compute_hpsv21(image_path: str | Path, prompt: str, _device: Any) -> float:
    _prepare_hps_checkpoint()
    import hpsv2

    # hpsv2.score normally returns a list of scores for the given image paths.
    # The official README documents hps_version="v2.1" for HPS v2.1.
    with warnings.catch_warnings():
        warnings.filterwarnings(
            "ignore",
            message=r"`torch\.cuda\.amp\.autocast\(args\.\.\.\)` is deprecated\..*",
            category=FutureWarning,
        )
        try:
            score = hpsv2.score([str(image_path)], prompt, hps_version=DEFAULT_HPS_VERSION)
        except TypeError:
            score = hpsv2.score([str(image_path)], prompt)
    return _to_scalar(score)


def _patch_t2v_metrics_cache_dir() -> None:
    """Force t2v_metrics to use the project HF cache when the package exposes constants."""
    try:
        import t2v_metrics

        constants = getattr(t2v_metrics, "constants", None)
        if constants is not None and hasattr(constants, "HF_CACHE_DIR"):
            constants.HF_CACHE_DIR = str(HF_CACHE_DIR)
    except Exception:
        # This is only a compatibility patch. The normal HF_HOME/HUGGINGFACE_HUB_CACHE
        # environment variables are already set above.
        pass


def _get_vqascore():
    global _VQASCORE_MODEL_OBJ
    if _VQASCORE_MODEL_OBJ is None:
        _patch_t2v_metrics_cache_dir()
        from t2v_metrics import VQAScore

        # Important:
        # t2v_metrics VQAScore does NOT accept a local snapshot directory as the
        # `model` argument. It expects the package's model alias, e.g.
        # `qwen3-vl-2b`. Offline resolution is controlled by HF_HOME and the
        # Hugging Face cache paths configured at the top of this file / shell script.
        model_candidates: List[str] = []
        for candidate in [
            DEFAULT_VQASCORE_MODEL,
            DEFAULT_VQASCORE_MODEL_ALIAS,
        ]:
            if candidate and candidate not in model_candidates:
                model_candidates.append(candidate)

        last_error: Optional[BaseException] = None
        errors: List[str] = []
        for model_candidate in model_candidates:
            try:
                _VQASCORE_MODEL_OBJ = VQAScore(model=model_candidate)
                break
            except Exception as exc:
                last_error = exc
                errors.append(f"model={model_candidate!r}: {type(exc).__name__}: {exc}")
                continue

        if _VQASCORE_MODEL_OBJ is None:
            detail = "\n".join(errors)
            raise RuntimeError(
                "Failed to initialize VQAScore. t2v_metrics expects a short model alias "
                "such as 'qwen3-vl-2b', not a local snapshot path. Tried:\n"
                f"{detail}\n"
                f"HF_HOME={HF_CACHE_DIR}\n"
                f"VQASCORE_LOCAL_MODEL={DEFAULT_VQASCORE_LOCAL_MODEL}"
            ) from last_error
    return _VQASCORE_MODEL_OBJ


def _compute_vqascore(image_path: str | Path, prompt: str, _device: Any) -> float:
    scorer = _get_vqascore()

    # t2v_metrics has used several call styles across examples/versions.
    # Try the common callable form first, then score/batch_score fallbacks.
    attempts: List[Tuple[str, Callable[[], Any]]] = [
        ("call_kwargs", lambda: scorer(images=[str(image_path)], texts=[prompt])),
        ("call_positional", lambda: scorer([str(image_path)], [prompt])),
        ("score_kwargs", lambda: scorer.score(images=[str(image_path)], texts=[prompt])),
        ("score_positional", lambda: scorer.score([str(image_path)], [prompt])),
        ("batch_score_kwargs", lambda: scorer.batch_score(images=[str(image_path)], texts=[prompt])),
    ]
    last_error: Optional[BaseException] = None
    for _name, fn in attempts:
        try:
            return _to_scalar(fn())
        except (TypeError, AttributeError) as exc:
            last_error = exc
            continue
    assert last_error is not None
    raise last_error


MetricFunc = Callable[[str | Path, str, Any], float]

METRIC_FUNCTIONS: Dict[str, MetricFunc] = {
    "OpenCLIPScore": _compute_openclip,
    "CNCLIPScore": _compute_cnclip,
    "AestheticScore": _compute_aesthetic,
    "MUSIQScore": _compute_musiq,
    "ImageRewardScore": _compute_imagereward,
    "HPSv2.1Score": _compute_hpsv21,
    "VQAScore": _compute_vqascore,
}

ALIASES: Dict[str, str] = {
    "openclip": "OpenCLIPScore",
    "openclipscore": "OpenCLIPScore",
    "cnclip": "CNCLIPScore",
    "cnclipscore": "CNCLIPScore",
    "aesthetic": "AestheticScore",
    "aestheticscore": "AestheticScore",
    "musiq": "MUSIQScore",
    "musiqscore": "MUSIQScore",
    "imagereward": "ImageRewardScore",
    "imagerewardscore": "ImageRewardScore",
    "hps": "HPSv2.1Score",
    "hpsv2": "HPSv2.1Score",
    "hpsv21": "HPSv2.1Score",
    "hpsv2.1": "HPSv2.1Score",
    "hpsv2.1score": "HPSv2.1Score",
    "vqa": "VQAScore",
    "vqascore": "VQAScore",
}

LEGACY_RESULT_KEYS = {
    "OpenCLIPScore": "OpenCLIP",
    "CNCLIPScore": "CNCLIP",
    "AestheticScore": "Aesthetic",
    "MUSIQScore": "MUSIQ",
    "ImageRewardScore": "ImageReward",
    "HPSv2.1Score": "HPSv2.1",
    "VQAScore": "VQAScore",
}


def normalize_metrics(metrics: Optional[Union[str, Sequence[str]]] = None) -> List[str]:
    if metrics is None:
        return ["OpenCLIPScore", "AestheticScore", "MUSIQScore"]
    if isinstance(metrics, str):
        raw_items = [m.strip() for m in metrics.split(",") if m.strip()]
    else:
        raw_items = [str(m).strip() for m in metrics if str(m).strip()]
    normalized: List[str] = []
    for item in raw_items:
        key = ALIASES.get(item.replace("_", "").replace("-", "").lower(), item)
        if key not in METRIC_FUNCTIONS:
            raise ValueError(f"Unsupported metric: {item}. Supported metrics: {', '.join(METRIC_FUNCTIONS)}")
        if key not in normalized:
            normalized.append(key)
    return normalized


def score_one_image(
    image_path: str | Path,
    prompt: str,
    language: Language,
    metrics: Optional[Union[str, Sequence[str]]] = None,
    device: Optional[str] = None,
) -> Dict[str, float]:
    _normalize_language(language)
    device_obj = _get_device(device)
    selected_metrics = normalize_metrics(metrics)
    results: Dict[str, float] = {}
    for metric_name in selected_metrics:
        results[metric_name] = METRIC_FUNCTIONS[metric_name](image_path, prompt, device_obj)
    return results


def objective_score(
    image_path: str | Path,
    prompt: str,
    language: Language,
    device: Optional[str] = None,
    metrics: Optional[Union[str, Sequence[str]]] = None,
    legacy_keys: bool = True,
) -> Dict[str, float]:
    """Compute objective metrics for a single image.

    By default this keeps compatibility with the older code by returning
    OpenCLIP/Aesthetic/MUSIQ keys when the old three metrics are used.
    Set legacy_keys=False to return final Excel column names such as
    OpenCLIPScore and AestheticScore.
    """

    scores = score_one_image(image_path, prompt, language, metrics=metrics, device=device)
    if not legacy_keys:
        return scores
    return {LEGACY_RESULT_KEYS.get(k, k): v for k, v in scores.items()}


# -----------------------------------------------------------------------------
# Directory worker
# -----------------------------------------------------------------------------


def iter_image_records(image_dir: Union[str, Path]) -> Iterable[Dict[str, Any]]:
    image_dir = Path(image_dir).expanduser().resolve()
    if not image_dir.exists():
        raise FileNotFoundError(f"Directory does not exist: {image_dir}")
    if not image_dir.is_dir():
        raise NotADirectoryError(f"Not a directory: {image_dir}")

    for img_path in sorted(image_dir.iterdir()):
        if not img_path.is_file():
            continue
        if img_path.suffix.lower() not in IMAGE_SUFFIXES:
            continue
        if ".warm." in img_path.name:
            continue
        match = FILENAME_PATTERN.match(img_path.name)
        if not match:
            continue

        txt_path = img_path.with_suffix(".txt")
        if not txt_path.exists():
            raise FileNotFoundError(f"Missing corresponding txt file: {txt_path}")
        txt_info = _read_sidecar_txt(txt_path)
        prompt_text = _pick_first_value(txt_info, ["prompt_text", "prompt", "text"], default=None)
        if not prompt_text:
            raise ValueError(f"prompt_text/prompt was not found in {txt_path}")

        info = match.groupdict()
        yield {
            "filename": img_path.name,
            "image_path": str(img_path),
            "prompt": str(prompt_text),
            "language": info["language"].lower(),
        }


def score_image_dir_to_jsonl(
    image_dir: Union[str, Path],
    output_jsonl: Union[str, Path],
    metrics: Union[str, Sequence[str]],
    device: Optional[str] = None,
    fail_fast: bool = False,
) -> Path:
    selected_metrics = normalize_metrics(metrics)
    gpu_index = None
    if device:
        device_match = re.fullmatch(r"cuda:(\d+)", device.strip().lower())
        if device_match and not os.environ.get("CUDA_VISIBLE_DEVICES", "").strip():
            gpu_index = int(device_match.group(1))
    output_path = Path(output_jsonl).expanduser().resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    records = list(iter_image_records(image_dir))
    total = len(records)
    print(f"[objective_score] images={total}, metrics={','.join(selected_metrics)}", flush=True)

    if records:
        warmup_record = records[0]
        for metric_name in selected_metrics:
            print(
                f"[objective_score] warmup: metric={metric_name}, "
                f"image={warmup_record['filename']}",
                flush=True,
            )
            try:
                score_one_image(
                    image_path=warmup_record["image_path"],
                    prompt=warmup_record["prompt"],
                    language=warmup_record["language"],
                    metrics=[metric_name],
                    device=device,
                )
            except Exception as exc:
                message = f"{type(exc).__name__}: {exc}"
                if fail_fast:
                    raise RuntimeError(f"Warmup failed for {metric_name}: {message}") from exc
                print(f"[objective_score] warmup warning: {metric_name}: {message}", flush=True)
            else:
                print(f"[objective_score] warmup complete: {metric_name}", flush=True)

    with output_path.open("w", encoding="utf-8") as f:
        for idx, rec in enumerate(records, start=1):
            scores: Dict[str, Optional[float]] = {}
            errors: Dict[str, str] = {}
            measurements: Dict[str, Dict[str, Any]] = {}
            print(f"[objective_score] {idx}/{total}: {rec['filename']}", flush=True)
            for metric_name in selected_metrics:
                monitor = None
                monitor_error: Optional[str] = None
                try:
                    from resource_monitor import ProcessResourceMonitor

                    sample_interval = float(os.environ.get("OBJECTIVE_RESOURCE_SAMPLE_INTERVAL_SEC", "0.5"))
                    monitor = ProcessResourceMonitor(sample_interval=sample_interval, gpu_index=gpu_index)
                    monitor.start()
                except Exception as exc:
                    monitor_error = f"{type(exc).__name__}: {exc}"

                started_at = datetime.now(timezone.utc)
                started_perf = time.perf_counter()
                try:
                    result = score_one_image(
                        image_path=rec["image_path"],
                        prompt=rec["prompt"],
                        language=rec["language"],
                        metrics=[metric_name],
                        device=device,
                    )
                    scores[metric_name] = result[metric_name]
                except Exception as exc:
                    errors[metric_name] = f"{type(exc).__name__}: {exc}"
                    scores[metric_name] = None
                    if fail_fast:
                        raise
                finally:
                    elapsed_seconds = time.perf_counter() - started_perf
                    ended_at = datetime.now(timezone.utc)
                    if monitor is not None:
                        try:
                            monitor.stop()
                            measurement = monitor.summary(
                                started_at_utc=started_at.isoformat(),
                                ended_at_utc=ended_at.isoformat(),
                                elapsed_sec=elapsed_seconds,
                            )
                        except Exception as exc:
                            monitor_error = f"{type(exc).__name__}: {exc}"
                            measurement = {
                                "started_at_utc": started_at.isoformat(),
                                "ended_at_utc": ended_at.isoformat(),
                                "elapsed_seconds": round(elapsed_seconds, 4),
                                "sample_count": 0,
                            }
                    else:
                        monitor_error = monitor_error or "Resource monitor could not be started."
                        measurement = {
                            "started_at_utc": started_at.isoformat(),
                            "ended_at_utc": ended_at.isoformat(),
                            "elapsed_seconds": round(elapsed_seconds, 4),
                            "sample_count": 0,
                        }
                    if monitor_error:
                        measurement["monitor_error"] = monitor_error
                    measurements[metric_name] = measurement
            out_rec = {
                "filename": rec["filename"],
                "image_path": rec["image_path"],
                "scores": scores,
                "errors": errors,
                "measurements": measurements,
            }
            f.write(json.dumps(out_rec, ensure_ascii=False) + "\n")
            f.flush()

    print(f"[objective_score] wrote: {output_path}", flush=True)
    return output_path


def print_local_model_paths() -> None:
    """Print the exact local model paths used by this script."""

    paths = {
        "MODELS_DIR": MODELS_DIR,
        "OpenCLIP": DEFAULT_OPENCLIP_PRETRAINED,
        "CNCLIP": _resolve_cnclip_checkpoint() if any(
            p.exists()
            for p in [
                Path(DEFAULT_CNCLIP_CKPT).expanduser() if DEFAULT_CNCLIP_CKPT else Path("/__missing__"),
                DEFAULT_CNCLIP_ROOT.expanduser() / DEFAULT_CNCLIP_CKPT_FILENAME,
                DEFAULT_CNCLIP_ROOT.expanduser() / DEFAULT_CNCLIP_MODEL_NAME / DEFAULT_CNCLIP_CKPT_FILENAME,
                MODELS_DIR / "cnclip" / DEFAULT_CNCLIP_MODEL_NAME / DEFAULT_CNCLIP_CKPT_FILENAME,
            ]
        ) else MODELS_DIR / "cnclip" / "ViT-L-14" / DEFAULT_CNCLIP_CKPT_FILENAME,
        "Aesthetic CLIP": DEFAULT_AESTHETIC_CLIP_MODEL,
        "Aesthetic head": DEFAULT_AESTHETIC_HEAD_PATH,
        "MUSIQ torch cache": TORCH_CACHE_DIR,
        "ImageReward checkpoint": DEFAULT_IMAGEREWARD_CKPT,
        "ImageReward med_config": DEFAULT_IMAGEREWARD_MED_CONFIG,
        "HPS v2.1 checkpoint": DEFAULT_HPSV21_CKPT,
        "HPS_ROOT": HPS_ROOT_DIR,
        "VQAScore alias": DEFAULT_VQASCORE_MODEL_ALIAS,
        "VQAScore local model": DEFAULT_VQASCORE_LOCAL_MODEL,
        "HF_HOME": HF_CACHE_DIR,
    }
    print("[objective_score] local model path check:")
    for name, path in paths.items():
        p = Path(path).expanduser()
        exists = p.exists()
        print(f"  - {name}: {p}  [{'OK' if exists else 'MISSING'}]")


def main() -> int:
    parser = argparse.ArgumentParser(description="Compute objective metric scores for one image or a whole image directory.")
    parser.add_argument("image_path", nargs="?", help="Single image path, used by legacy single-image mode.")
    parser.add_argument("prompt", nargs="?", help="Prompt text, used by legacy single-image mode.")
    parser.add_argument("language", nargs="?", help="chinese/english, used by legacy single-image mode.")
    parser.add_argument("--image-dir", default=None, help="Score all valid images in this directory and write JSONL.")
    parser.add_argument("--metrics", default="OpenCLIPScore,AestheticScore,MUSIQScore", help="Comma-separated metrics.")
    parser.add_argument("--output-jsonl", default=None, help="Output JSONL path for --image-dir mode.")
    parser.add_argument("--device", default=None, help="cuda, cuda:0 or cpu. Default: auto.")
    parser.add_argument("--fail-fast", action="store_true", help="Stop immediately when a metric fails.")
    parser.add_argument("--final-keys", action="store_true", help="Single-image mode: print final Excel metric names instead of legacy names.")
    parser.add_argument("--print-local-model-paths", action="store_true", help="Print exact local model paths and exit.")
    args = parser.parse_args()

    if args.print_local_model_paths:
        print_local_model_paths()
        return 0

    if args.image_dir:
        if not args.output_jsonl:
            parser.error("--output-jsonl is required when --image-dir is used.")
        score_image_dir_to_jsonl(
            image_dir=args.image_dir,
            output_jsonl=args.output_jsonl,
            metrics=args.metrics,
            device=args.device,
            fail_fast=args.fail_fast,
        )
        return 0

    if not args.image_path or args.prompt is None or args.language is None:
        parser.error("Single-image mode requires: image_path prompt language")

    scores = objective_score(
        image_path=args.image_path,
        prompt=args.prompt,
        language=args.language,
        device=args.device,
        metrics=args.metrics,
        legacy_keys=not args.final_keys,
    )
    print(json.dumps(scores, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
