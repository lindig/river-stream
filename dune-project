(lang dune 3.0)

(name river-stream)

(generate_opam_files true)

(source
 (github lindig/river-stream))

(license Unlicense)

(authors "Christian Lindig <lindig@gmail.com>")

(maintainers "Christian Lindig <lindig@gmail.com>")

(package
 (name river-stream)
 (synopsis "Estimate the stream of a river from rowing FIT files")
 (description
  "Reads GPS speed and stroke rate from FIT files and estimates, per file, the speed of the river's stream from the difference between the two directions of travel, JSON output.")
 (depends fit ocaml cmdliner angstrom yojson ptime))
