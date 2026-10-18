"""
EVSEdetails.py

Collects a snapshot of the details of the EVSEs in Switzerland and saves it as
ChargingStationDetails_DD-MM-YYYY.csv in the folder given as first argument.
One row per entry in an EVSE's ChargingFacilities list (one row with empty
power fields if there is none).
"""

import os
import sys
from datetime import datetime

import pandas as pd
import requests

URL = ("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/"
       "data/ch.bfe.ladestellen-elektromobilitaet.json")


def flatten(data):
    records = []
    # "EVSEData" holds one container per operator, each with "EVSEDataRecord"
    for container in data.get("EVSEData", []):
        for item in container.get("EVSEDataRecord", []):
            address = item.get("Address", {})
            base = {
                "EvseID": item.get("EvseID"),
                "ChargingStationId": item.get("ChargingStationId"),
                "Accessibility": item.get("Accessibility"),
                "GeoCoordinates": item.get("GeoCoordinates", {}).get("Google"),
                "City": address.get("City"),
                "Country": address.get("Country"),
                "PostalCode": address.get("PostalCode"),
                "Street": address.get("Street"),
                "ParkingSpot": address.get("ParkingSpot"),
                "ParkingFacility": address.get("ParkingFacility"),
                "DynamicInfoAvailable": item.get("DynamicInfoAvailable"),
                "IsOpen24Hours": item.get("IsOpen24Hours"),
                "LastUpdate": item.get("lastUpdate"),
                "MaxCapacity": item.get("MaxCapacity"),
                "DynamicPowerLevel": item.get("DynamicPowerLevel"),
                "EnergySource": item.get("EnergySource"),
            }
            facilities = item.get("ChargingFacilities", []) or [{}]
            for fac in facilities:
                record = base.copy()
                record["ChargingFacility_power"] = fac.get("power")
                record["ChargingFacility_powertype"] = fac.get("powertype")
                records.append(record)
    return pd.DataFrame(records)


def main():
    out_dir = sys.argv[1] if len(sys.argv) > 1 else "."
    response = requests.get(URL, timeout=60)
    response.raise_for_status()
    df = flatten(response.json())
    out = os.path.join(
        out_dir, "ChargingStationDetails_%s.csv" % datetime.today().strftime("%d-%m-%Y"))
    df.to_csv(out, index=False)
    print("%d rows, %d unique EvseIDs -> %s" % (len(df), df["EvseID"].nunique(), out))


if __name__ == "__main__":
    main()
