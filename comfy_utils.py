#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import copy
import json
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Tuple

import requests

from config import (
    API_WIDGET_KEYS,
    COMFY_BASE_URL,
    GENERATION_TIMEOUT_SEC,
    MODEL_CONFIG,
    NEGATIVE_PROMPT,
    POLL_INTERVAL_SEC,
    UI_WIDGET_INDEX,
)
from resource_monitor import ResourceMonitor


def utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def load_json(json_path: str) -> Dict[str, Any]:
    with open(json_path, "r", encoding="utf-8") as f:
        return json.load(f)


def save_binary(content: bytes, save_path: Path) -> None:
    save_path.parent.mkdir(parents=True, exist_ok=True)
    with open(save_path, "wb") as f:
        f.write(content)


def save_text_json(obj: Dict[str, Any], save_path: Path) -> None:
    save_path.parent.mkdir(parents=True, exist_ok=True)
    with open(save_path, "w", encoding="utf-8") as f:
        json.dump(obj, f, ensure_ascii=False, indent=2)


def make_filename(
    model: str,
    prompt_no: str,
    language: str,
    width: int,
    height: int,
    step: int,
    seed_no: int,
    is_warmup: bool = False,
) -> str:
    base = f"c_{model}_{prompt_no}_{language}_{width}-{height}_step{step}_{seed_no}"
    if is_warmup:
        return f"{base}.warm.png"
    return f"{base}.png"


def find_node(workflow_ui: Dict[str, Any], node_id: int) -> Dict[str, Any]:
    for node in workflow_ui.get("nodes", []):
        if int(node.get("id")) == int(node_id):
            return node
    raise KeyError(f"workflow Mapping ID:{node_id}")


def set_widget_value(node: Dict[str, Any], widget_index: int, value: Any) -> None:
    widgets = node.setdefault("widgets_values", [])
    while len(widgets) <= widget_index:
        widgets.append(None)
    widgets[widget_index] = value


def edit_workflow_ui(
    model_name: str,
    workflow_ui: Dict[str, Any],
    prompt_text: str,
    width: int,
    height: int,
    step: int,
    seed: int,
    file_prefix: str,
) -> Dict[str, Any]:
    cfg = MODEL_CONFIG[model_name]
    wf = copy.deepcopy(workflow_ui)

    set_widget_value(find_node(wf, cfg["prompt_node_id"]), UI_WIDGET_INDEX["clip_text_encode.text"], prompt_text)
    set_widget_value(find_node(wf, cfg["negative_prompt_node_id"]), UI_WIDGET_INDEX["clip_text_encode.text"], NEGATIVE_PROMPT)
    set_widget_value(find_node(wf, cfg["latent_node_id"]), UI_WIDGET_INDEX["latent.width"], int(width))
    set_widget_value(find_node(wf, cfg["latent_node_id"]), UI_WIDGET_INDEX["latent.height"], int(height))

    if model_name in ("zimage", "qwen"):
        sampler_node = find_node(wf, cfg["sampler_node_id"])
        set_widget_value(sampler_node, UI_WIDGET_INDEX["ksampler.seed"], int(seed))
        set_widget_value(sampler_node, UI_WIDGET_INDEX["ksampler.control_after_generate"], "fixed")
        set_widget_value(sampler_node, UI_WIDGET_INDEX["ksampler.steps"], int(step))
    elif model_name == "sdxl":
        sampler_node = find_node(wf, cfg["sampler_node_id"])
        steps_node = find_node(wf, cfg["steps_node_id"])
        set_widget_value(sampler_node, UI_WIDGET_INDEX["samplercustom.seed"], int(seed))
        set_widget_value(sampler_node, UI_WIDGET_INDEX["samplercustom.control_after_generate"], "fixed")
        set_widget_value(steps_node, UI_WIDGET_INDEX["sdturbo_scheduler.steps"], int(step))
    else:
        raise ValueError(f"InfoModel:{model_name}")

    set_widget_value(find_node(wf, cfg["save_node_id"]), UI_WIDGET_INDEX["saveimage.filename_prefix"], file_prefix)
    return wf


def workflow_ui_to_api_prompt(workflow_ui: Dict[str, Any]) -> Dict[str, Any]:
    """
    InfoskippedInfo, Info Note, PreviewImage.
    """
    non_executable_node_types = {"Note", "PreviewImage"}

    links = workflow_ui.get("links", [])
    link_map: Dict[int, Tuple[int, int, int, int, str]] = {}
    for item in links:
        if len(item) >= 6:
            link_id = int(item[0])
            link_map[link_id] = (int(item[1]), int(item[2]), int(item[3]), int(item[4]), item[5])

    executable_node_ids = {
        int(node["id"])
        for node in workflow_ui.get("nodes", [])
        if node["type"] not in non_executable_node_types
    }

    api_prompt: Dict[str, Any] = {}
    for node in workflow_ui.get("nodes", []):
        node_type = node["type"]
        if node_type in non_executable_node_types:
            continue
        if node_type not in API_WIDGET_KEYS:
            raise ValueError(f"InfoPromptType:{node_type}")

        node_id = str(int(node["id"]))
        inputs: Dict[str, Any] = {}

        for input_item in node.get("inputs", []):
            link_id = input_item.get("link")
            if link_id is None:
                continue
            src_node_id, src_slot, _dst_node, _dst_slot, _dtype = link_map[int(link_id)]
            if int(src_node_id) not in executable_node_ids:
                continue
            inputs[input_item["name"]] = [str(src_node_id), int(src_slot)]

        widget_keys = API_WIDGET_KEYS[node_type]
        widget_values = node.get("widgets_values", []) or []
        for idx, key in enumerate(widget_keys):
            if idx < len(widget_values):
                inputs[key] = widget_values[idx]

        api_prompt[node_id] = {"class_type": node_type, "inputs": inputs}

    return api_prompt


def queue_prompt(api_prompt: Dict[str, Any]) -> str:
    client_id = str(uuid.uuid4())
    payload = {"prompt": api_prompt, "client_id": client_id}
    resp = requests.post(f"{COMFY_BASE_URL}/prompt", json=payload, timeout=60)
    resp.raise_for_status()
    data = resp.json()
    if "prompt_id" not in data:
        raise RuntimeError(f"ComfyUI /prompt ReturnInfo:{data}")
    return data["prompt_id"]


def wait_for_completion(prompt_id: str, poll_interval: float = POLL_INTERVAL_SEC, timeout_sec: int = GENERATION_TIMEOUT_SEC) -> Dict[str, Any]:
    start = time.time()
    while True:
        if time.time() - start > timeout_sec:
            raise TimeoutError(f"Info ComfyUI Info, prompt_id={prompt_id}")
        resp = requests.get(f"{COMFY_BASE_URL}/history/{prompt_id}", timeout=60)
        resp.raise_for_status()
        data = resp.json()
        if prompt_id in data:
            return data[prompt_id]
        time.sleep(poll_interval)


def extract_images_from_history(history_item: Dict[str, Any]) -> List[Dict[str, str]]:
    outputs = history_item.get("outputs", {})
    images: List[Dict[str, str]] = []
    for _, node_output in outputs.items():
        for img in node_output.get("images", []):
            if all(k in img for k in ("filename", "subfolder", "type")):
                images.append({"filename": img["filename"], "subfolder": img["subfolder"], "type": img["type"]})
    return images


def download_image(image_info: Dict[str, str]) -> bytes:
    resp = requests.get(f"{COMFY_BASE_URL}/view", params=image_info, timeout=300)
    resp.raise_for_status()
    return resp.content


def t2i(
    model_name: str,
    workflow_json_path: str,
    prompt_no: str,
    prompt_text: str,
    language: str,
    width: int,
    height: int,
    step: int,
    seed: int,
    seed_no: int,
    output_dir: str,
    is_warmup: bool = False,
):
    workflow_ui = load_json(workflow_json_path)
    filename = make_filename(model_name, prompt_no, language, width, height, step, seed_no, is_warmup=is_warmup)
    final_img_path = Path(output_dir) / filename
    final_txt_path = final_img_path.with_suffix(".txt")
    file_prefix = final_img_path.stem

    edited_ui = edit_workflow_ui(model_name, workflow_ui, prompt_text, width, height, step, seed, file_prefix)
    api_prompt = workflow_ui_to_api_prompt(edited_ui)


    monitor = ResourceMonitor()
    started_at = utc_now_iso()
    start_ts = time.time()
    end_ts = start_ts
    ended_at = started_at

    monitor.start()
    try:
        prompt_id = queue_prompt(api_prompt)
        history_item = wait_for_completion(prompt_id)
        images = extract_images_from_history(history_item)
        if not images:
            raise RuntimeError(f"Info ComfyUI history Mapping, prompt_id={prompt_id}")
        img_bytes = download_image(images[-1])
        save_binary(img_bytes, final_img_path)
        end_ts = time.time()
        ended_at = utc_now_iso()
    finally:
        monitor.stop()

    monitor_record = {
        "record_type": "t2i_generation_monitor",
        "record_version": 1,
        "is_warmup": bool(is_warmup),
        "model": model_name,
        "prompt_id_excel": str(prompt_no),
        "language": language,
        "prompt_text": prompt_text,
        "resolution": {"width": int(width), "height": int(height)},
        "step": int(step),
        "seed": int(seed),
        "seed_no": int(seed_no),
        "image_path": str(final_img_path),
        "workflow_json_path": str(workflow_json_path),
        "comfy_base_url": COMFY_BASE_URL,
        "negative_prompt": NEGATIVE_PROMPT,
        "monitor": monitor.summary(started_at, ended_at, end_ts - start_ts),
    }
    save_text_json(monitor_record, final_txt_path)
    return final_img_path, final_txt_path
