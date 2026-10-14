#!/usr/bin/env python3
"""Rebuild minimal FIT files from the output of `fit --records`.

Only the "record" message with timestamp, latitude, longitude, speed and
cadence is written. This exists to create test fixtures when only the JSON
conversion of a FIT file is at hand.

usage: json_to_fit.py INPUT.json[.gz] OUTDIR [--name FILE=SLICE ...]
"""
import gzip, json, os, struct, sys

FIT_EPOCH = 631065600
_CRC = [0x0000, 0xCC01, 0xD801, 0x1400, 0xF001, 0x3C00, 0x2800, 0xE401,
        0xA001, 0x6C00, 0x7800, 0xB401, 0x5000, 0x9C01, 0x8801, 0x4400]


def crc16(data, crc=0):
    for b in data:
        for nib in (b & 0xF, b >> 4):
            tmp = _CRC[crc & 0xF]
            crc = (crc >> 4) & 0x0FFF
            crc = crc ^ tmp ^ _CRC[nib]
    return crc


def semicircles(deg):
    return round(deg * 2147483648.0 / 180.0)


def encode(records):
    definition = struct.pack("<BBBHB", 0x40, 0, 0, 20, 5)
    # (field number, size, base type): timestamp, lat, lon, speed, cadence
    for num, size, base in ((253, 4, 0x86), (0, 4, 0x85), (1, 4, 0x85),
                            (6, 2, 0x84), (4, 1, 0x02)):
        definition += struct.pack("<BBB", num, size, base)
    body = definition
    for r in records:
        body += struct.pack("<BIiiHB", 0x00,
                            int(round(r["timestamp"])) - FIT_EPOCH,
                            semicircles(r["latitude"]),
                            semicircles(r["longitude"]),
                            int(round(r["speed"] * 1000)),
                            int(round(r["cadence"])))
    header = struct.pack("<BBHI4s", 14, 0x20, 2132, len(body), b".FIT")
    header += struct.pack("<H", crc16(header))
    return header + body + struct.pack("<H", crc16(header + body))


def complete(r):
    return all(r.get(k) is not None
               for k in ("timestamp", "latitude", "longitude", "speed", "cadence"))


def main(argv):
    src, outdir = argv[1], argv[2]
    opener = gzip.open if src.endswith(".gz") else open
    with opener(src, "rt") as fh:
        files = json.load(fh)
    os.makedirs(outdir, exist_ok=True)
    for entry in files:
        recs = [r for r in entry["records"] if complete(r)]
        recs.sort(key=lambda r: r["timestamp"])
        out = os.path.join(outdir, os.path.basename(entry["file"]))
        with open(out, "wb") as fh:
            fh.write(encode(recs))
        print(out, len(recs))


if __name__ == "__main__":
    main(sys.argv)
