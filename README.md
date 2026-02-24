# dzarn

A static analysis tool for OCaml 5.4 that detects unused public functions and checks cyclomatic complexity.

## Features

- Recursively scans directories for `.ml` / `.mli` files
- Automatically distinguishes between public and private functions
- Reports unused public functions
- `--fix` option to automatically remove them
- Cyclomatic complexity checker
- Configurable via sexp-format configuration file

## Installation

```bash
opam install dzarn
```

Or build from source:

```bash
git clone <repository>
cd dzarn
opam install . --deps-only
make build
```

## Usage

### Basic Usage

```bash
dune exec -- bin/main.exe <project-directory>
```

Or after installation:

```bash
dzarn <project-directory>
```

### Configuration File

You can configure dzarn using a sexp-format configuration file (default: `dzarn.sexp`):

```sexp
((unused_enabled true)
 (complexity_enabled true)
 (complexity_threshold 10))
```

Options:
- `--config <file>` or `-c <file>` - Specify configuration file (default: `dzarn.sexp`)

Configuration options:
- `unused_enabled <bool>` - Enable/disable unused function checking (default: true)
- `complexity_enabled <bool>` - Enable/disable complexity checking (default: true)
- `complexity_threshold <int>` - Complexity threshold (default: 10)

### Output Examples

```
Unused function 'unused_helper' in src/lib.ml:42
Unused function 'old_api' in src/api.ml:15
Function 'complex_func' has complexity 15 (threshold: 10) in src/lib.ml:50
```

If issues are found, the exit code is `1`. If no issues are found, the exit code is `0`.

### `--fix` Option

Automatically remove unused functions:

```bash
dzarn --fix <project-directory>
```

**Note**: `--fix` removes functions using the AST, which may change code formatting. It's recommended to create a backup before using this option.

## Public Function Detection Rules

dzarn determines whether a function is public using the following rules:

1. **When `.mli` file exists**: Only functions declared in the `.mli` are considered public
2. **When only `.ml` file exists**:
   - `let name = ...` → Public function
   - `let _name = ...` → Private function (underscore prefix)

### Example

```ocaml
(* lib.ml *)
let public_func x = x + 1        (* Public function *)
let _private_func x = x + 2      (* Private function - not reported *)

(* When lib.mli exists *)
val public_func : int -> int    (* Declared as public *)
(* _private_func is not declared, so it's private *)
```

## Detected Usage Contexts

Functions called in the following contexts are considered "used":

- Regular function calls
- Calls in pattern match expressions
- Calls in record field expressions
- Functions passed as arguments to higher-order functions
- Calls in `try...with` handlers
- Calls in `try...with effect` (effect handler) bodies
- Loops, conditionals, sequences, and other various syntax constructs

## Cyclomatic Complexity

dzarn can check cyclomatic complexity of functions and report those exceeding the threshold. The complexity is calculated based on:

- Branching statements (`if`, `match`, `try`)
- Loops (`while`, `for`)
- Short-circuit operators

The default threshold is 10, but can be configured via the configuration file.

## Compatibility

- OCaml 5.4.0+

## License

MIT
