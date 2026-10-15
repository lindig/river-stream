![Build](https://github.com/lindig/river-stream/workflows/CI/badge.svg)

# river-stream

Estimate the stream (current) of a river from rowing FIT files. Each file is
treated as one session and gets its own estimate, emitted as JSON.

## Build

Needs OCaml >= 4.14, dune >= 3.0 and the opam packages `cmdliner`, `angstrom`,
`yojson` and `ptime`. The `fit` library
(<https://github.com/lindig/fit>, MIT) is vendored in `vendor/fit/`.

    dune build          # also generates river-stream.opam from dune-project
    dune test
    ./_build/default/bin/main.exe --help      # or: dune exec river-stream -- ...

## Use

    river-stream session1.fit session2.fit > streams.json
    river-stream --downstream-bearing 225 --max-cadence 45 workout.fit

The river's direction of flow is not recorded in a FIT file. `--downstream-bearing`
gives the compass bearing (0 = north, 90 = east) in which the river flows on
average; it defines "downstream". The default is north, which is right for the
Cam from Cambridge towards Ely. A negative stream with the warning
"check the downstream bearing" usually means the bearing is the wrong way round.

Options (see `--help`): `--min-speed` (3 m/s), `--max-speed` (6 m/s),
`--max-cadence` (40 strokes/min; `inf` disables), `--exponent` (1/3), `--window` (15 s), `--min-points` (20 per
direction), `--bootstrap` (500), `--block` (60 s), `--seed`, `--max-size`,
`--compact`.

Exit status: 0 on success (including files for which no stream can be
estimated), 1 if a file could not be read, 124 for invalid options.

## Method

1. Records with timestamp, position, speed and stroke rate are kept.
2. Positions are projected to local metres; the principal axis of the track is
   taken as the river axis and oriented towards `--downstream-bearing`.
3. A point is *downstream* or *upstream* by the sign of the slope of its
   along-axis position over a +/- 15 s window.
4. Points with speed in [`--min-speed`, `--max-speed`] (3 to 6 m/s) and a
   stroke rate in (0, `--max-cadence`] (up to 40 strokes/min) are used.
5. Least squares for `speed = k * cadence^p + d * u` with d = +1 downstream and
   -1 upstream; `u` is the stream (positive = flowing downstream), `k` a crew
   constant, `p` = 1/3 unless changed.
6. Intervals: normal approximation, and a block bootstrap (60 s blocks) that
   respects the autocorrelation of GPS samples.

**No stream can be estimated when all movement is in one direction.** Then the
file is reported with `"status": "no_estimate"`, `"reason": "one_direction_only"`
and `"stream": null`. The same status is used for `no_usable_points` (nothing
passes the filters, e.g. no stroke rate in the file) and `too_few_points` (fewer
than `--min-points` in one direction).

## Output

    {
      "tool": "river-stream", "version": "1.1.0", "convention": "...",
      "parameters": { ... },
      "files": [
        { "file": "...", "status": "ok",
          "start": "2026-10-12 17:32:28 UTC", "end": "...", "duration": "1:12:24",
          "stream": { "speed_ms", "speed_kmh", "std_error",
                      "ci95_normal": [lo, hi],
                      "ci95_bootstrap": { "low", "high", "replicates" } },
          "model": { "exponent", "k", "k_std_error", "residual_sd_ms", "points" },
          "data": { "records", "complete_records",
                    "points_with_direction": {...}, "points_used": {...} },
          "directions": { "downstream": {...}, "upstream": {...} },
          "diagnostics": { "best_exponent", "stream_at_best_exponent_ms",
                           "shared_cadence_range", "warnings": [...] } },
        { "file": "...", "status": "no_estimate", "start": "...", "end": "...",
          "duration": "...", "reason": "...",
          "message": "...", "stream": null, "data": {...} },
        { "file": "...", "status": "error", "message": "..." } ] }

`start`, `end` and `duration` come from the first and last timestamp of any
record in the FIT file. FIT timestamps are UTC, so these are UTC times (in
British Summer Time, local clock time is one hour later). They are also present
for `no_estimate` entries.

`best_exponent` is the exponent in [0.05, 1.5] with the smallest misfit, a check
on the v ~ s^(1/3) assumption. `shared_cadence_range` is the stroke rate range
(5th to 95th percentile) used in both directions; the stream is identified by
comparing both directions at equal effort.

## Limitations

* The river axis is a straight line through the track (principal axis). That is
  fine for an out-and-back row on a meandering river, not for loops or a basin.
* The stream is assumed constant over the session and equal for both directions;
  wind and varying crew effort within a direction are absorbed into the
  residual.
* The `fit` parser does not handle every FIT file (for example
  `coros-dura-medium-20260424150313.fit` in its repository). Such a file is
  reported with `"status": "error"`; the parser itself also prints diagnostics
  to stderr.
* `tools/json_to_fit.py` rebuilds minimal FIT files from the JSON produced by
  `fit --records`; it was used to create the Norwich test fixtures.
