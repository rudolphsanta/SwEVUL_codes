"""
evse_status_downloader.py

Saves the SFOE real-time EVSE status file every five minutes (minutes ending
in 0 or 5, plus 30 s) to <SWEVUL_RAW>/YYYY-MM/data-YYYY-MM-DD-HH-MM-SS.json.
File names use the local time of the machine.
"""

import json
import os
import time
from datetime import datetime, timedelta

import requests

# Root folder for the raw snapshots (monthly sub-folders are created in it)
ROOT_FOLDER = os.environ.get("SWEVUL_RAW", "Swiss-EVChg-Data")

# SFOE real-time status feed
URL = ("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/"
       "status/ch.bfe.ladestellen-elektromobilitaet.json")

REQUEST_TIMEOUT = 30  # seconds


def get_next_time(interval_minutes=5):
    """Unix timestamp of the next clock time aligned to the interval, +30 s."""
    now = datetime.now().replace(second=0, microsecond=0)
    next_minute = (now.minute // interval_minutes + 1) * interval_minutes
    if next_minute >= 60:
        next_time = (now + timedelta(hours=1)).replace(minute=0)
    else:
        next_time = now.replace(minute=next_minute)
    return (next_time + timedelta(seconds=30)).timestamp()


def run_collector_loop(interval_minutes=5):
    # Wait until the first aligned run time before starting
    sleep_duration = get_next_time(interval_minutes) - time.time()
    if sleep_duration > 0:
        time.sleep(sleep_duration)

    while True:
        now = datetime.now()

        try:
            response = requests.get(URL, timeout=REQUEST_TIMEOUT)
            response.raise_for_status()
            json_data = response.json()
        except Exception as e:
            # retry after 30 s
            print(f"[{datetime.now()}] Error fetching data: {e}")
            time.sleep(30)
            continue

        folder_path = os.path.join(ROOT_FOLDER, now.strftime("%Y-%m"))
        os.makedirs(folder_path, exist_ok=True)
        full_file_path = os.path.join(
            folder_path, now.strftime("data-%Y-%m-%d-%H-%M-%S.json"))

        with open(full_file_path, "w", encoding="utf-8") as f:
            json.dump(json_data, f, ensure_ascii=False, indent=2)
        print(f"[{datetime.now()}] Saved: {full_file_path}")

        sleep_duration = get_next_time(interval_minutes) - time.time()
        if sleep_duration > 0:
            time.sleep(sleep_duration)


if __name__ == "__main__":
    run_collector_loop(interval_minutes=5)
