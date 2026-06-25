#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
import os
import sys
import time
import random
import subprocess
from pathlib import Path
from typing import Any, Dict, List, Tuple

import pandas as pd
import requests

from comfy_utils import t2i
from config import COMFY_BASE_URL, COMFY_PORT, MODEL_CONFIG

MODEL_ORDER = ("qwen", "zimage", "sdxl")

REQUIRED_PROMPT_COLUMNS = {
    "prompt_id",
    "set_type",
    "prompt_cn",
    "prompt_en",
}

MODEL_PARAMS: Dict[str, Dict[str, int]] = {
    "qwen": {"width": 1024, "height": 1024, "step": 28},
    "sdxl": {"width": 768, "height": 768, "step": 4},
    "zimage": {"width": 768, "height": 768, "step": 9},
}

SEED_COUNT = 3

COMFY_WORKDIR = "/home/chenll/ComfyUI"
COMFY_START_CMD = ["python", "main.py", "--listen", "0.0.0.0", "--port", "8188"]
COMFY_READY_TIMEOUT_SEC = 180
COMFY_STOP_TIMEOUT_SEC = 30


_rng = random.SystemRandom()


def safe_str(v: Any) -> str:
    if pd.isna(v):
        return ""
    return str(v).strip()


def ensure_exists(path: str) -> None:
    if not os.path.exists(path):
        raise FileNotFoundError(f"File does not exist:{path}")


def is_comfy_ready() -> bool:
    try:
        resp = requests.get(f"{COMFY_BASE_URL}/system_stats", timeout=3)
        return resp.status_code == 200
    except Exception:
        return False


def is_port_open(host: str = "127.0.0.1", port: int = COMFY_PORT) -> bool:
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.settimeout(1.0)
        return sock.connect_ex((host, port)) == 0


def stop_comfyui() -> None:
    try:
        subprocess.run(
            ["pkill", "-f", r"python main\.py --listen 0\.0\.0\.0 --port 8188"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except Exception:
        pass

    start = time.time()
    while time.time() - start < COMFY_STOP_TIMEOUT_SEC:
        if not is_port_open(port=COMFY_PORT):
            return
        time.sleep(1)

    raise RuntimeError("Stopping ComfyUI timed out; port 8188 is still open.")


def start_comfyui() -> None:
    log_path = Path(COMFY_WORKDIR) / "comfyui_restart.log"
    log_f = open(log_path, "a", encoding="utf-8")

    subprocess.Popen(
        COMFY_START_CMD,
        cwd=COMFY_WORKDIR,
        stdout=log_f,
        stderr=log_f,
        start_new_session=True,
    )

    start = time.time()
    while time.time() - start < COMFY_READY_TIMEOUT_SEC:
        if is_comfy_ready():
            return
        time.sleep(2)

    raise RuntimeError(f"Starting ComfyUI timed out: not ready within {COMFY_READY_TIMEOUT_SEC}s.")


def restart_comfyui() -> None:
    print("=" * 80)
    print("[COMFYUI] restarting ...")
    stop_comfyui()
    start_comfyui()
    print("[COMFYUI] ready")
    print("=" * 80)


def build_prompt_tasks(df: pd.DataFrame) -> List[Tuple[str, str, str]]:
    tasks: List[Tuple[str, str, str]] = []
    for _, row in df.iterrows():
        pid = safe_str(row["prompt_id"])
        for lang, text in [
            ("chinese", safe_str(row["prompt_cn"])),
            ("english", safe_str(row["prompt_en"])),
        ]:
            if text:
                tasks.append((pid, lang, text))
    return tasks


def random_seed() -> int:
    return _rng.randrange(10**14, 10**15)


def get_first_task(model: str, tasks: List[Tuple[str, str, str]]):
    pid, lang, text = tasks[0]
    params = MODEL_PARAMS[model]
    seed_no = 0
    seed = random_seed()
    return pid, lang, text, params["width"], params["height"], params["step"], seed, seed_no


def warm_once(model: str, workflow: str, task, output_dir: str):
    pid, lang, text, w, h, step, seed, seed_no = task
    print(f"[WARM] {model} ...")
    return t2i(
        model_name=model,
        workflow_json_path=workflow,
        prompt_no=pid,
        prompt_text=text,
        language=lang,
        width=w,
        height=h,
        step=step,
        seed=seed,
        seed_no=seed_no,
        output_dir=output_dir,
        is_warmup=True,
    )


def run_batch(excel_path: str, output_dir: str, dataset: str) -> None:
    ensure_exists(excel_path)
    Path(output_dir).mkdir(parents=True, exist_ok=True)

    df = pd.read_excel(excel_path, sheet_name="prompts")
    missing_columns = REQUIRED_PROMPT_COLUMNS.difference(df.columns)
    if missing_columns:
        missing = ", ".join(sorted(missing_columns))
        raise ValueError(f"Missing required prompt columns: {missing}")

    set_col = df["set_type"].astype(str).str.strip().str.lower()
    df = df[set_col == dataset]

    tasks = build_prompt_tasks(df)
    if not tasks:
        raise ValueError(f"No {dataset} task")

    restart_comfyui()
    print("=== GREENLIGHT TEST ===")
    for model in MODEL_ORDER:
        wf = MODEL_CONFIG[model]["json_path"]
        task = get_first_task(model, tasks)
        try:
            warm_once(model, wf, task, output_dir)
        except Exception as e:
            raise RuntimeError(f"Green-light test failed: {model} {e}")

    print("=== GREENLIGHT OK ===")

    total = len(tasks) * len(MODEL_ORDER) * SEED_COUNT
    done = 0

    for model in MODEL_ORDER:
        wf = MODEL_CONFIG[model]["json_path"]
        params = MODEL_PARAMS[model]
        w = params["width"]
        h = params["height"]
        step = params["step"]

        restart_comfyui()

        task = get_first_task(model, tasks)
        warm_once(model, wf, task, output_dir)

        for pid, lang, text in tasks:
            for seed_no in range(SEED_COUNT):
                seed = random_seed()
                done += 1
                print(
                    f"[{done}/{total}] model={model}, ID={pid}, lang={lang}, "
                    f"res={w}x{h}, step={step}, seed_no={seed_no}, seed={seed}"
                )
                try:
                    img_path, txt_path = t2i(
                        model_name=model,
                        workflow_json_path=wf,
                        prompt_no=pid,
                        prompt_text=text,
                        language=lang,
                        width=w,
                        height=h,
                        step=step,
                        seed=seed,
                        seed_no=seed_no,
                        output_dir=output_dir,
                        is_warmup=False,
                    )
                    print(f"    -> image: {img_path}")
                    print(f"    -> monitor: {txt_path}")
                except Exception as e:
                    print(f"ERROR: {e}", file=sys.stderr)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="batch text-to-image generation script")
    parser.add_argument("excel_path", help="Excel file path")
    parser.add_argument("output_dir", help="output directory")
    parser.add_argument("dataset", help="Dataset(human_rated_core or objective_only_extended)")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    run_batch(args.excel_path, args.output_dir, str(args.dataset).lower())
