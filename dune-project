(lang dune 2.0)
(name csexp)

(license MIT)
(maintainers "Jeremie Dimino <jeremie@dimino.org>")
(authors
  "Quentin Hocquet <mefyl@gruntech.org>"
  "Jane Street Group, LLC <opensource@janestreet.com>"
  "Jeremie Dimino <jeremie@dimino.org>")
(source (github ocaml/dune))
(documentation "https://dune.readthedocs.io/")

(generate_opam_files true)

(package
 (name csexp)
 (synopsis "Parsing and printing of Canonical S-expressions")
 (description "
This library provides minimal support for Canonical S-expressions
[1]. Canonical S-expressions, or csexp for short are a binary encoding
of s-expressions that is super simple and well suited for
communication between programs.

The library provides a fully generic parser that can be used with any
kind of inputs or s-expression types as well as a generic serialiser.

[1] https://en.wikipedia.org/wiki/Canonical_S-expressions
"))
