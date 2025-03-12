# smol

![use OCaml now](https://i.imgur.com/GDeYO4m.png)

Authors/Maintainers:
- 22125113 - Nguyen Quang Truong
- 22125078 - Dinh Cao Minh Quan
- 22125121 - Dinh Hoang Viet
- 22125051 - Pham Phu Loi

we vibe code a smol ocaml based language

## Features

- [x] Variables
- [x] Functions
    - [x] Recursion
    - [x] Mutual Recursion
    - [x] Higher order functions
    - [x] Closures
- [x] Basic integer arithmetic operations (`+`, `-`, `*`, `/`)
- [x] Basic boolean operations (`&&`, `||`, `!`)
- [x] Control flow
- [ ] Type inference

## Installation

- [Install OCaml](https://ocaml.org/docs/install.html)
- Clone this repository and enter the directory
- Create an OPAM switch
    ```sh
    opam switch create . ocaml-base-compiler.5.3.0
    ```
- Install dependencies
    ```sh
    opam install . --deps-only
    ```
- Build the project
    ```sh
    dune build
    ```
- Run the project
    ```sh
    dune exec smol <path-to-file>
    ```

## Examples

See the `examples/` directory for example programs.

For example, to compile `examples/1.smol`:

```sh
dune exec smol examples/1.smol
```
