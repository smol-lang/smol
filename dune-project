(lang dune 3.17)

(name smol)

(generate_opam_files true)

(source
 (github smol-lang/smol))

(authors
  "Nguyen Quang Truong <nqtruong22@apcs.fitus.edu.vn>"
  "Dinh Cao Minh Quan <dcmquan22@apcs.fitus.edu.vn>"
  "Dinh Hoang Viet <dhviet22@apcs.fitus.edu.vn>"
  "Pham Phu Loi <pploi22@apcs.fitus.edu.vn>")

(maintainers
  "Nguyen Quang Truong <nqtruong22@apcs.fitus.edu.vn>"
  "Dinh Cao Minh Quan <dcmquan22@apcs.fitus.edu.vn>"
  "Dinh Hoang Viet <dhviet22@apcs.fitus.edu.vn>"
  "Pham Phu Loi <pploi22@apcs.fitus.edu.vn>")

(license MIT)

(package
 (name smol)
 (synopsis "a smol ocaml-based language")
 (depends ocaml dune ppx_deriving))

(using menhir 3.0)
