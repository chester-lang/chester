# Chester Development Documentation

This section contains technical documentation for Chester's implementation details and development notes.

> [!NOTE]
> These documents are intended for both humans and LLMs to understand Chester's internals.

## Quick Links

- **[Module Structure](module-structure.md)** - Codebase organization and architecture
- **[TypeScript Backend](typescript-backend.md)** - How Chester compiles to TypeScript
- **[Go Backend](go-backend.md)** - Go backend status and implementation
- **[Type Checking System](type-checking-system.md)** - Type inference and checking
- **[Elaboration System](elaboration-system.md)** - The elaboration algorithm
- **[Effect System](effects-system.md)** - Effect tracking and CPS transformation

## Current Implementation

Chester is implemented in Scala 3.8.1 with multi-platform support:

- **JVM**: Standard Java Virtual Machine target
- **JavaScript**: Via Scala.js for browser/Node.js
- **Native**: Via Scala Native for compiled binaries

**Key Components:**
- Parser and tokenizer
- Type checker with constraint solving
- TypeScript code generator (fully implemented)
- Go code generator (in progress)
- LSP server for IDE integration
- IntelliJ IDEA plugin

## Contributing

When adding development documentation:

1. Create markdown files in `docs/src/dev/`
2. Follow existing documentation style
3. Include code examples where appropriate
4. Link to relevant source files using `file://` links
5. Update `SUMMARY.md` to include new pages
 
