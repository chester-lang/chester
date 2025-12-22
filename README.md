# Chester Programming Language

Chester is a statically-typed functional language that compiles to TypeScript, featuring an advanced effect system and seamless JavaScript/TypeScript interoperability.

> [!WARNING]
> Chester is under active development. The language design and implementation are not stable.

## Key Features

- **TypeScript Backend** - Compiles to readable TypeScript code
- **Effect System** - Track and manage side effects with CPS transformation  
- **JS/TS Interop** - Import npm packages with automatic type extraction
- **Multi-Platform** - Runs on JVM, JavaScript, and Native
- **Developer Tools** - REPL, LSP server, IntelliJ plugin, web REPL

## Quick Start

### Build

```bash
sbt cliJVM/assembly
```

### Run

```bash
# Start REPL
java -jar modules/cli/jvm/target/scala-3.7.4/cli-assembly-*.jar

# Run a file
java -jar modules/cli/jvm/target/scala-3.7.4/cli-assembly-*.jar hello.chester

# Compile to TypeScript
java -jar modules/cli/jvm/target/scala-3.7.4/cli-assembly-*.jar ts src/ --output dist
```

For detailed setup, see **[Getting Started](docs/src/guide/getting-started.md)** in the documentation.

## Documentation

📚 Full documentation is available in the `docs/` directory:

- **[Getting Started](docs/src/guide/getting-started.md)** - Build and run your first program
- **[CLI Usage](docs/src/guide/cli-usage.md)** - Command-line interface guide
- **[Language Guide](docs/src/guide/)** - Syntax and features
- **[Development Docs](docs/src/dev/)** - Internals and architecture

### Build Documentation Site

```bash
cd docs
./dev.sh build
# or serve locally:
./dev.sh serve
```

## Website (Next.js + pnpm)

The marketing site lives in `site/` and uses the latest Next.js and React stack.

```bash
cd site
pnpm install
pnpm dev   # start the dev server
pnpm build # production build
```

You can also run the same commands from sbt:

```bash
sbt "site/pnpmDev"   # dev server
sbt "site/pnpmBuild" # production build
```

### REPL bundle (shared CLI logic)

The browser REPL reuses the CLI. Build the Scala.js bundle and copy it into `site/public/scala`:

```bash
sbt webRepl/copyWebRepl
```

Then run the site (`pnpm dev`) and open `/repl`. If the bundle is missing, the page will show an error with the command to build it.

To run the site tests (including the REPL mock), build the bundle first and then:

```bash
cd site
pnpm test
```

## Repository Structure

```
chester/
├── modules/           # Source code
│   ├── cli/          # Command-line interface
│   ├── core/         # Parser, type checker, backends
│   ├── lsp/          # Language Server Protocol
│   ├── utils/        # Shared utilities
│   ├── web-repl/     # Browser REPL (Scala.js)
│   └── intellij-plugin/  # IntelliJ integration
├── site/             # Next.js website
├── docs/             # Documentation (mdbook)
├── vendor/           # Vendored dependencies
└── build.sbt         # Build configuration
```

See **[Module Structure](docs/src/dev/module-structure.md)** for details.

## Example

```chester
module example;

import "chalk";  // Import npm packages

def greet(name: String): IO Unit = {
  let message = chalk.green("Hello, " ++ name ++ "!");
  println(message)
};

def main: IO Unit = greet("World");
```

Compile to TypeScript:

```bash
chester ts example.chester --output dist
```

## Development

- **Language**: Scala 3.7.4
- **Build Tool**: sbt
- **Platforms**: JVM, JavaScript (Scala.js), Native (Scala Native)

```bash
# Compile all modules
sbt compile

# Run tests
sbt test

# Build CLI
sbt cliJVM/assembly

# Build native binary (faster startup)
sbt cliNative/nativeLink
```

## Links

- **Previous Version**: https://github.com/chester-lang/chester2025draft (IDE plugins are for the previous version)
- **Website Domain**: https://the-lingo.org/ (subject to change)

## License

See [LICENSE.md](LICENSE.md)

