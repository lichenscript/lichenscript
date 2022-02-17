# Contributing Guide

## Repo Setup
Make sure `opam` and `dune` are all available in your computer. If not, follow the steps below:
1. Following the [installation guide](https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system) from opam to install it properly.
2. Following the tips in terminal for opam setup (basically it's `opam init`).
3. Installing dune:
   ```bash
   opam install dune
   ```
4. Installing libs under repo's root:
   ```bash
   opam install .
   ```
5. Building:
   ```bash
   dune build
   ```

## Running Tests
- Run all tests:
  ```bash
  ./test.sh
  ```
- Run specific test case:
  ```bash
  ./test.sh -N ${test_case_name}
  ```
  > ${test_case_name} refers to an arbitray folder name under `./example/` 

- Run specific platform:
   ```bash
   FLAGS="--platform js" ./test.sh
   ```
