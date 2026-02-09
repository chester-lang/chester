# CLI Usage

The Chester command-line interface provides tools for running, compiling, and formatting Chester code.

## Installation

After building Chester (see [Getting Started](getting-started.md)), you can run the CLI via:

```bash
# Using the JAR
java -jar modules/cli/jvm/target/scala-*/cli-assembly-*.jar

# Or create an alias
alias chester='java -jar modules/cli/jvm/target/scala-*/cli-assembly-*.jar'

# Or use the native binary
modules/cli/native/target/scala-*/cli-out
```

## Commands

### REPL (Interactive Mode)

Start the Read-Eval-Print Loop:

```bash
chester
```

**REPL Commands:**

| Command | Description | Example |
|---------|-------------|---------|
| `:t <expr>` or `:type <expr>` | Show the type of an expression | `:t 42` |
| `:l <file>` or `:load <file>` | Load and evaluate a Chester file | `:l hello.chester` |
| `:q` or `:quit` | Exit the REPL | `:q` |

**Example REPL Session:**

```
chester> def square(x: Int): Int = x * x
=> [function]

chester> square(7)
=> 49

chester> :t square
Type: Int -> Int

chester> :l examples/math.chester
Loaded examples/math.chester.

chester> :quit
Goodbye.
```

### Run a File

Execute a Chester file:

```bash
chester <file>
```

**Example:**

```bash
chester hello.chester
```

This will:
1. Parse and elaborate the file
2. Type-check the code
3. Apply effect CPS transformation if needed
4. Evaluate the result
5. Print the output

### Compile to TypeScript

Generate TypeScript code from Chester source:

```bash
chester ts <input> [--output <directory>]
```

**Arguments:**
- `<input>` - Chester source file or directory
- `--output <directory>` - Output directory (default: `ts-out` for directories, or `<filename>.ts` for files)

**Examples:**

```bash
# Compile a single file
chester ts math.chester --output lib

# Compile all .chester files in a directory
chester ts src/ --output dist
```

**Output Structure:**

For a file `src/utils.chester`, the output will be:
- Single file: `dist/utils.ts`
- Directory: Each `.chester` file becomes a `.ts` file

### Compile and Show AST

Elaborate code and save the AST representation:

```bash
chester compile <input> [--output <file>]
```

This command:
1. Parses and elaborates the input
2. Type-checks the AST
3. Optionally saves the AST to a file

**Example:**

```bash
chester compile example.chester --output example.ast
```

The output shows:
```
Type: String
AST:
(def greet (lambda (name String) ...))
```

### Format Code

Format a Chester source file in-place:

```bash
chester format <file>
```

**Example:**

```bash
chester format messy.chester
```

This will:
1. Parse the file (preserving comments)
2. Pretty-print according to Chester style
3. Overwrite the original file

> [!WARNING]
> `format` overwrites the file in-place. Ensure you have backups or use version control.

### Version and Help

```bash
# Show version
chester --version

# Show help
chester --help
```

## Advanced Features

### JavaScript/TypeScript Imports

Chester automatically resolves TypeScript definitions for imported packages:

```chester
import "lodash";

def doubled: Array[Int] = lodash.map([1, 2, 3], (x) => x * 2);
```

**Requirements:**
- Package must be in `node_modules/`
- TypeScript definitions available (either bundled or via `@types/*`)

**Supported Import Styles:**

```chester
import "package-name";              // NPM package
import "package/submodule";         // Submodule
import "@scope/package";            // Scoped package
import "fs";                        // Node.js built-in (via @types/node)
```

Chester will:
1. Locate the package in `node_modules/`
2. Extract TypeScript definitions (`.d.ts` files)
3. Parse the definitions into Chester types
4. Make them available for use

### Effect CPS Transformation

When a function's type includes `IO` effects, Chester applies a Continuation-Passing Style transformation:

```chester
def main: IO Unit = {
  println("Starting...");
  let x = readLine();
  println("You entered: " ++ x)
};
```

The TypeScript output uses async/await or callbacks to properly sequence effects.

## Configuration and Cache

### TypeScript Definitions Cache

Downloaded npm package metadata and tarballs are cached in:

```
<working-directory>/js-typings/npm-cache/
```

To clear the cache:

```bash
rm -rf js-typings/
```

### Node.js Type Definitions

Chester looks for `@types/node` in:
1. `node_modules/@types/node/` (direct dependency)
2. `node_modules/.pnpm/@types+node@*/` (pnpm)

For Node.js built-ins to work, install:

```bash
npm install --save-dev @types/node
# or
pnpm add -D @types/node
```

## Troubleshooting

### "Input file does not exist"

Ensure the path is correct and the file has a `.chester` extension.

### TypeScript Import Errors

**Problem**: `Cannot find module 'some-package'`

**Solutions:**
1. Install the package: `npm install some-package`
2. Install types: `npm install @types/some-package`
3. Check that `node_modules/` exists in the working directory

### Performance Tips

For faster startup, use the native binary instead of the JAR:

```bash
sbt cliNative/nativeLink
```

### REPL Not Working

If the REPL doesn't start, check:
- Java version (requires Java 11+)
- Terminal supports ANSI escape codes
- Try running with `--no-colors` (future flag)

## Examples

### Complete Workflow

```bash
# 1. Create a Chester file
cat > calculate.chester << 'EOF'
module calculate;

import "chalk";

def add(a: Int, b: Int): Int = a + b;

def main: IO Unit = {
  let result = add(10, 32);
  let message = chalk.green("Result: " ++ toString(result));
  println(message)
};
EOF

# 2. Install dependencies
npm install chalk @types/chalk

# 3. Run it
chester calculate.chester

# 4. Compile to TypeScript
chester ts calculate.chester --output dist

# 5. Run the TypeScript output
node dist/calculate.ts
```

## Next Steps

- **[Language Syntax](syntax-grammar.md)** - Learn Chester's full grammar
- **[Effect System](../dev/effects-system.md)** - Understand IO and effect tracking
- **[TypeScript Backend](../dev/typescript-backend.md)** - How compilation works
