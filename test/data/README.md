# Test data

* `cnn-norwich-*.fit`: the two Norwich sessions on the River Cam. Only the JSON
  conversion (`fit --records`) of the originals was available, so these FIT
  files were rebuilt from it with `tools/json_to_fit.py` (records with
  timestamp, position, speed and cadence; timestamps have 1 s resolution as in
  FIT).
* `norwich-one-direction.fit`: first session up to the turning point (downstream
  only).
* `norwich-short-return.fit`: first session up to 32 records after the turning
  point (very short upstream leg).
* `norwich-tiny.fit`: first 10 records of the first session (too slow to pass the
  speed threshold).
* `SpeedCoach-GPS-Workout.fit`, `xpress-4x-2022-08-03.fit`, `carolin.fit`:
  unmodified samples from https://github.com/lindig/fit (MIT licence).
  `carolin.fit` has no stroke rate.
