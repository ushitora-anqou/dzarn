# dzarn

A static analysis tool for OCaml 5.4+ that detects unused public functions, checks cyclomatic complexity, enforces naming conventions, and checks function length.

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

- **Function Length Checker** - Identifies overly long functions
  - Configurable line count threshold (default: 50)
  - Detects both top-level and nested functions
  - Helps identify functions that may benefit from refactoring

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
  --json                        Output results in JSON format
  -v, --verbose                 Enable verbose output
  --help                        Show help message
```

### Configuration File

Configure dzarn using a sexp-format configuration file (default: `dzarn.sexp`):

```sexp
((unused_enabled true)
 (complexity_enabled true)
 (complexity_threshold 10)
 (naming_enabled true)
 (length_enabled true)
 (length_threshold 50)
 (unused_nolint_enabled true)
 (json_output false))
```

**Configuration options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `unused_enabled` | bool | `true` | Enable/disable unused function checking |
| `complexity_enabled` | bool | `true` | Enable/disable complexity checking |
| `complexity_threshold` | int | `10` | Complexity threshold for reporting |
| `naming_enabled` | bool | `true` | Enable/disable naming convention checking |
| `length_enabled` | bool | `true` | Enable/disable function length checking |
| `length_threshold` | int | `50` | Maximum number of lines before reporting |
| `unused_nolint_enabled` | bool | `true` | Enable/disable unused nolint directive detection |
| `json_output` | bool | `false` | Enable JSON output format |

### Output Examples

```bash
$ dzarn src/
Unused function 'helper' in src/util.ml:42
Unused function 'old_api' in src/api.ml:15
Function 'complex_func' has complexity 15 (threshold: 10) in src/lib.ml:50
Function 'long_function' has 75 lines (threshold: 50) in src/processor.ml:120
Naming violation: camelCase at src/utils.ml:5:4
variable/function name should be lowercase snake_case
Naming violation: `bad_variant at src/types.ml:12:20
polymorphic variant should be uppercase snake_case
Unused nolint directive at src/file.ml:10:1
  The [@@@nolint] attribute does not suppress any violations for linters: naming
No unused nolint directives found.
```

**Exit codes:**
- `0` - No issues found
- `1` - Issues were detected

### JSON Output

dzarn supports outputting results in JSON format for programmatic consumption in CI/CD pipelines or other tools.

#### Using JSON Output

```bash
dzarn --json <project-directory>
```

#### JSON Format

The JSON output contains an array of issues and a summary:

```json
{
  "issues": [
    {
      "type": "unused_function",
      "file": "src/util.ml",
      "line": 42,
      "column": 0,
      "message": "Unused function 'helper'",
      "module": "Util",
      "function": "helper"
    },
    {
      "type": "complexity",
      "file": "src/lib.ml",
      "line": 50,
      "column": 0,
      "message": "Function 'complex_func' has complexity 15 (threshold: 10)",
      "module": "Lib",
      "function": "complex_func",
      "complexity": 15,
      "threshold": 10
    },
    {
      "type": "naming",
      "file": "src/utils.ml",
      "line": 5,
      "column": 4,
      "message": "variable/function name should be lowercase snake_case",
      "name": "camelCase",
      "violation_type": "variable/function name should be lowercase snake_case"
    },
    {
      "type": "length",
      "file": "src/processor.ml",
      "line": 120,
      "column": 0,
      "message": "Function 'long_function' has 75 lines (threshold: 50)",
      "module": "Processor",
      "function": "long_function",
      "line_count": 75,
      "threshold": 50
    }
  ],
  "summary": {
    "total_issues": 4,
    "unused_functions": 1,
    "complexity": 1,
    "naming": 1,
    "length": 1
  }
}
```

#### Issue Types

| Type | Description | Optional Fields |
|------|-------------|-----------------|
| `unused_function` | Unused public function | `module`, `function` |
| `complexity` | Function exceeds complexity threshold | `module`, `function`, `complexity`, `threshold` |
| `naming` | Naming convention violation | `name`, `violation_type` |
| `length` | Function exceeds line count threshold | `module`, `function`, `line_count`, `threshold` |

#### Using with jq

You can pipe the JSON output to `jq` for filtering and formatting:

```bash
# Show only unused functions
dzarn --json src/ | jq '.issues[] | select(.type == "unused_function")'

# Show summary only
dzarn --json src/ | jq '.summary'

# Format as a table
dzarn --json src/ | jq -r '.issues[] | [.type, .file, .line, .message] | @tsv'
```

#### Configuration

JSON output can also be enabled via the configuration file:

```sexp
((json_output true))
```

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

## Function Length Checker

The length checker identifies functions that exceed a configurable line count threshold.

### How Line Count is Calculated

The line count is calculated from the function's definition start line to the end of its expression. This includes:

- Top-level functions defined with `let`
- Nested functions defined within `let ... in` expressions
- All lines between the start and end of the function's body

**Note:** For functions containing nested functions, the outer function's line count includes the lines of all nested functions. This is intentional - functions that contain many nested definitions are often candidates for refactoring.

### Example

```ocaml
(* This function is 12 lines long *)
let process_data input =
  let step1 x =
    let _ = x + 1 in
    let _ = x + 2 in
    let _ = x + 3 in
    let _ = x + 4 in
    let _ = x + 5 in
    x
  in
  step1 input
```

With `length_threshold = 10`, this would report:
```
Function 'process_data' has 12 lines (threshold: 10) in src/file.ml:1
Function 'step1' has 10 lines (threshold: 10) in src/file.ml:2
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

## Suppressing Linters with `[@nolint]` Attributes

dzarn supports three types of `nolint` attributes to selectively disable linters for specific parts of your code.

### Attribute Types

| Attribute | Syntax | Scope | Example |
|-----------|--------|-------|---------|
| Expression-level | `[@nolint "..."]` | Suppresses linter for the specific expression/pattern | `let (badVar [@nolint "naming"]) = ...` |
| Item-level | `[@@nolint "..."]` | Suppresses linter for the entire structure item | `let func x = x + 1 [@@nolint "naming"]` |
| File-level | `[@@@nolint "..."]` | Suppresses linter for all subsequent items in the file | `[@@@nolint "naming"]` |

### Supported Linter Names

You can specify one or more linters to disable:

| Linter Name | Description |
|-------------|-------------|
| `"naming"` | Naming convention linter |
| `"complexity"` | Cyclomatic complexity checker |
| `"length"` | Function length checker |
| `"unused"` | Unused function detector |
| `"all"` | All linters |

### Examples

#### Expression-level `[@nolint "..."]`

Suppress linter for a specific variable or expression:

```ocaml
let test_function () =
  let (badLocal [@nolint "naming"]) = 5 in  (* Only badLocal is suppressed *)
  badLocal + 1
```

#### Item-level `[@@nolint "..."]`

Suppress linter for an entire function definition:

```ocaml
let anotherBadFunction x = x + 3 [@@nolint "naming"]  (* Entire function is suppressed *)

(* This is NOT suppressed *)
let camelCaseFunction x = x + 2  (* Will be reported *)
```

#### File-level `[@@@nolint "..."]`

Suppress linter for all subsequent items in the file:

```ocaml
[@@@nolint "naming"]  (* Suppresses naming violations for all code below *)

let shouldBeSuppressed1 x = x + 1  (* Not reported *)
let shouldBeSuppressed2 y = y * 2  (* Not reported *)
let alsoSuppressed z = z + 3      (* Not reported *)
```

#### Multiple Linters

Disable multiple linters at once:

```ocaml
let complexFunction x [@@nolint "naming" "complexity"] =
  let (badVar [@nolint "naming" "length"]) = x in
  badVar + 1
```

#### Disable All Linters

```ocaml
[@@@nolint "all"]  (* Disables all linters for the rest of the file *)
```

## Detecting Unused `[@nolint]` Directives

dzarn can detect `[@nolint]` attributes that don't actually suppress any violations. This helps keep your codebase clean by identifying obsolete suppression directives that are no longer needed.

### How It Works

The unused nolint detector tracks which nolint attributes were **actually used** to suppress a violation, then reports any nolints that were never marked as "used".

### Configuration

Enable or disable unused nolint detection via the configuration file:

```sexp
((unused_nolint_enabled true))
```

### Example

```ocaml
(* This nolint is unused - there's no naming violation *)
let good_name x = x + 1 [@@nolint "naming"]

(* This nolint IS used - suppresses naming violation *)
let camelCase x = x + 2 [@@nolint "naming"]
```

When running dzarn on this file with `unused_nolint_enabled` set to `true`:

```bash
$ dzarn src/
Unused nolint directive at src/file.ml:2:1
  The [@@@nolint] attribute does not suppress any violations for linters: naming
Naming violation: camelCase at src/file.ml:5:1
variable/function name should be lowercase snake_case
```

### Notes

- A nolint attribute with multiple linters (e.g., `[@nolint "naming" "complexity"]`) is only reported as unused if **none** of the specified linters would report a violation
- The `"all"` linter name is considered used if **any** linter would report a violation at that location
- If a linter is disabled in the configuration, its nolints may be reported as unused since that linter won't report any violations

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
