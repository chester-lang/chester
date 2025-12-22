# Getting Started with Chester

This guide will help you build Chester from source and write your first program.

## Prerequisites

Before building Chester, ensure you have:

- **Java 11 or later** (for running sbt and the JVM build)
- **sbt** (Scala Build Tool) - [Installation guide](https://www.scala-sbt.org/download.html)
- **Node.js 18+** and **pnpm** (optional, for the website and web REPL)

## Building from Source

Clone the Chester repository:

```bash
git clone https://github.com/chester-lang/chester.git
cd chester
```

### Build the CLI (JVM)

```bash
sbt cliJVM/assembly
```

This creates an executable JAR at `modules/cli/jvm/target/scala-3.7.4/cli-assembly-*.jar`.

For convenience, create an alias:

```bash
alias chester='java -jar modules/cli/jvm/target/scala-3.7.4/cli-assembly-*.jar'
```

### Build the Native Binary (Optional)

For faster startup, build a native executable:

```bash
sbt cliNative/nativeLink
```

The binary will be at `modules/cli/native/target/scala-3.7.4/cli-out`.

## Your First Chester Program

Create a file called `hello.chester`:

```chester
module hello;

def greet(name: String): String = 
  "Hello, " ++ name ++ "!";

def main: IO Unit = 
  println(greet("Chester"));
```

Run it:

```bash
chester hello.chester
```

## Using the REPL

Start the interactive REPL:

```bash
chester
```

Try some expressions:

```
chester> 2 + 3
=> 5

chester> def double(x: Int): Int = x * 2
chester> double(21)
=> 42

chester> :t "hello"
Type: String
```

### REPL Commands

- `:t <expr>` - Show the type of an expression
- `:l <file>` - Load a Chester file
- `:q` or `:quit` - Exit the REPL

## Compiling to TypeScript

Chester's primary backend compiles to TypeScript. Create `example.chester`:

```chester
module math;

def add(a: Int, b: Int): Int = a + b;
def multiply(a: Int, b: Int): Int = a * b;

export def calculate: Int = 
  add(10, multiply(5, 3));
```

Compile to TypeScript:

```bash
chester ts example.chester --output ts-out
```

This generates `ts-out/example.ts`:

```typescript
export function add(a: number, b: number): number {
  return a + b;
}

export function multiply(a: number, b: number): number {
  return a * b;
}

export const calculate: number = add(10, multiply(5, 3));
```

## Using JavaScript/TypeScript Libraries

Chester can import npm packages automatically. Create `web-demo.chester`:

```chester
module web;

import "chalk";

def main: IO Unit = {
  let red = chalk.red("Error:");
  println(red ++ " Something went wrong!")
};
```

Before running, install the package:

```bash
npm install chalk @types/chalk
```

Then run:

```bash
chester web-demo.chester
```

Chester will:
1. Detect the `import "chalk"` statement
2. Find `node_modules/chalk` and read its TypeScript definitions
3. Generate appropriate Chester type signatures
4. Type-check your code against the library

## Next Steps

- **[CLI Usage Guide](cli-usage.md)** - Learn all CLI commands and options
- **[Language Syntax](syntax-grammar.md)** - Understand Chester's grammar
- **[Effect System](../dev/effects-system.md)** - Learn about effect tracking
- **[TypeScript Backend](../dev/typescript-backend.md)** - How compilation works

## Troubleshooting

### "Command not found: chester"

Make sure you've created the alias or use the full path to the JAR/binary.

### TypeScript Import Errors

Ensure the package has TypeScript definitions:
- Install `@types/<package>` if available
- Or use packages that bundle their own `.d.ts` files

### Build Errors

Try cleaning and rebuilding:

```bash
sbt clean
sbt cliJVM/assembly
```

## Development Setup

For contributors, see the [Development Documentation](../dev/README.md) for information on:
- Module structure
- Running tests
- LSP and IntelliJ plugin setup
- Building the website
