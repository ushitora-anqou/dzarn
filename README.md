# dzarn

A static analysis tool for OCaml 5.4+ that detects unused public functions, checks cyclomatic complexity, and enforces naming conventions.

## Features

- **Dead Code Analyzer** - Detects unused public functions
  - Recursively scans directories for `.ml` / `.mli` files
  - Automatically distinguishes between public and private functions
  - `--fix` option to automatically remove unused functions
  - Tracks function calls in various contexts (pattern matching, records, higher-order functions, effect handlers, etc.)

- **Cyclomatic Complexity Checker** - Identifies overly complex functions
  - Configurable complexity threshold (default: 10)
  - Calculates complexity based on branching, loops, and control flow

- **Naming Convention Linter** - Enforces consistent naming patterns
  - Variables/functions: `lowercase_snake_case`
  - Variant constructors: `Uppercase_snake_case`
  - Exceptions: `Uppercase_snake_case`
  - Checks function parameters, lambda parameters, and local variables

## Installation

```bash
opam install dzarn
```

Or build from source:

```bash
git clone https://github.com/ushitora-anqou/dzarn.git
cd dzarn
opam install . --deps-only
make build
```

## Usage

### Basic Usage

```bash
# Analyze a project directory
dzarn <project-directory>

# After building from source
dune exec -- bin/main.exe <project-directory>
```

### Command-Line Options

```bash
dzarn [OPTIONS] <project-directory>

Options:
  -c <file>, --config <file>    Specify configuration file (default: dzarn.sexp)
  --fix                         Remove unused functions automatically
  -v, --verbose                 Enable verbose output
  --help                        Show help message
```

### Configuration File

Configure dzarn using a sexp-format configuration file (default: `dzarn.sexp`):

```sexp
((unused_enabled true)
 (complexity_enabled true)
 (complexity_threshold 10)
 (naming_enabled true))
```

**Configuration options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `unused_enabled` | bool | `true` | Enable/disable unused function checking |
| `complexity_enabled` | bool | `true` | Enable/disable complexity checking |
| `complexity_threshold` | int | `10` | Complexity threshold for reporting |
| `naming_enabled` | bool | `true` | Enable/disable naming convention checking |

### Output Examples

```bash
$ dzarn src/
Unused function 'helper' in src/util.ml:42
Unused function 'old_api' in src/api.ml:15
Function 'complex_func' has complexity 15 (threshold: 10) in src/lib.ml:50
Naming violation: camelCase at src/utils.ml:5:4
variable/function name should be lowercase snake_case
Naming violation: `bad_variant at src/types.ml:12:20
polymorphic variant should be uppercase snake_case
```

**Exit codes:**
- `0` - No issues found
- `1` - Issues were detected

### `--fix` Option

Automatically remove unused functions:

```bash
dzarn --fix <project-directory>
```

**Note**: `--fix` removes functions using the AST, which may change code formatting. It's recommended to review the changes or create a backup before using this option.

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

- Regular function calls (`f x`)
- Calls in pattern match expressions
- Calls in record field expressions
- Functions passed as arguments to higher-order functions
- Calls in `try...with` handlers
- Calls in `try...with effect` (effect handler) bodies
- Qualified module calls (`Module.function`)
- Loops, conditionals, sequences, and other various syntax constructs

## Cyclomatic Complexity

dzarn calculates cyclomatic complexity based on:

- Branching statements (`if`, `match`, `try`)
- Loops (`while`, `for`)
- Short-circuit operators (`&&`, `||`, `||`)

Functions exceeding the threshold are reported with their complexity score:

```ocaml
Function 'complex_func' has complexity 15 (threshold: 10) in src/lib.ml:50
```

## Naming Convention Linter

The naming linter enforces the following conventions:

### Naming Rules

| Category | Convention | Valid Examples | Invalid Examples |
|----------|------------|-----------------|------------------|
| Variables | `lowercase_snake_case` | `my_var`, `local_value` | `myVar`, `camelCase` |
| Functions | `lowercase_snake_case` | `my_func`, `calculate_result` | `myFunc`, `PascalCase` |
| Variant constructors | `Uppercase_snake_case` | `My_case`, `Another_variant` | `my_case`, `CamelCase` |
| Exceptions | `Uppercase_snake_case` | `My_exception`, `Error_code` | `my_exception`, `BadException` |

### Example Violations

```ocaml
(* Variables/functions - should be lowercase_snake_case *)
let camelCase x = x + 1          (* ❌ Violation: should be camel_case *)
let good_function x = x + 1       (* ✅ Valid *)

(* Variant constructors - should be Uppercase_snake_case *)
type t = Lower_case | CamelCase   (* ❌ Violation: should be Lower_case *)
type t = Lower_case | Good_case  (* ✅ Valid *)

(* Exceptions - should be Uppercase_snake_case *)
exception badException            (* ❌ Violation: should be Bad_exception *)
exception Good_exception          (* ✅ Valid *)

(* Private names (underscore prefix) are skipped *)
let _privateHelper x = x         (* ✅ Not checked *)
```

## Limitations

### Polymorphic Variants

Polymorphic variant (`` `variant ``) checking has limitations due to OCaml's AST structure:

- Variant names in type declarations are not currently checked
- Pattern matching and expressions with polymorphic variants are not checked
- Regular variants (defined with `type`) are fully supported

This is due to the complexity of extracting variant names from `string loc` in OCaml's Parsetree representation.

## Compatibility

- OCaml 5.4.0+
- Tested with OCaml 5.4+

## Development

### Running Tests

```bash
make test
```

### Building

```bash
make build
```

## License

MIT

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues on GitHub.

## Author

ushitora-anqou
