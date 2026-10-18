"""
ladestationen_scheduled.py

Scheduled download of charging station files, run by a scheduler (e.g. cron).
Every run saves the status file to <SWEVUL_RAW>/data_dynamic/; runs between
08:00-08:01 and 20:00-20:01 also save the description file to data_static/.
File names use the local time of the machine (no time-zone handling).
"""

import json
import os
import traceback
from datetime import datetime

import requests

BASE_DIR = os.environ.get("SWEVUL_RAW", "charging_stations")
DIR_DYNAMIC = os.path.join(BASE_DIR, "data_dynamic")
DIR_STATIC = os.path.join(BASE_DIR, "data_static")
ERROR_LOG = os.path.join(BASE_DIR, "error_log.txt")

URL_BASE = "https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet"
URL_DYNAMIC = URL_BASE + "/status/oicp/ch.bfe.ladestellen-elektromobilitaet.json"
URL_STATIC = URL_BASE + "/data/oicp/ch.bfe.ladestellen-elektromobilitaet.json"

REQUEST_TIMEOUT = 30  # seconds


def download(url, filename):
    response = requests.get(url, timeout=REQUEST_TIMEOUT)
    response.raise_for_status()
    response.encoding = "utf-8"
    with open(filename, "w") as outfile:
        json.dump(json.loads(response.text), outfile)


def main():
    # One timestamp per run, so file names and the static-file decision agree
    now = datetime.now()
    stamp = now.strftime("%Y-%m-%d-%H-%M-%S")
    os.makedirs(DIR_DYNAMIC, exist_ok=True)
    os.makedirs(DIR_STATIC, exist_ok=True)

    try:
        # Dynamic file: every run
        download(URL_DYNAMIC, os.path.join(DIR_DYNAMIC, "data-%s.json" % stamp))

        # Static file: twice per day, at 08:00 and 20:00
        current_time = int(now.strftime("%H%M%S"))
        if 80000 <= current_time <= 80100 or 200000 <= current_time <= 200100:
            download(URL_STATIC, os.path.join(
                DIR_STATIC, "ch.bfe.ladestellen-elektromobilitaet_%s.json" % stamp))

    except Exception:
        # Log failures so that gaps in the data can be diagnosed later
        with open(ERROR_LOG, "a") as log_file:
            log_file.write("%s\n%s\n" % (now, traceback.format_exc()))


if __name__ == "__main__":
    main()
