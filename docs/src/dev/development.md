# Development Memo

NOTE THAT ALL CODE AND DOCUMENTS CAN AND WILL BE OUTDATED OR CONFLICTING. ANALYSE INFORMATION AVAIABLE TO YOU CAREFULLY

## Documentation Guidelines

1. **Avoid Meaningless Adjectives**
   - ❌ **NEVER USE** subjective qualifiers like: "better", "improved", "good", "great", "enhanced", "advanced", "beautiful", "powerful", "robust", "excellent", "high-quality"
   - ✅ **ALWAYS USE** factual, specific, measurable descriptions
   - Describe concrete characteristics and behaviors
   - Focus on correctness and functionality first

2. **Focus on Facts**
   - Document what something IS, not how "good" you think it is
   - Identify concrete capabilities and limitations
   - Omit subjective assessments and emotional language (BUT EMPHASIZE ON WHAT ALWAYS DID WRONG BY DEVELOPERS IS OK LIKE EMOJI EMPHASIZE AND UPPERCASE AND **BOLD**)
   - Avoid superlatives and value judgments

3. **Eliminate Fluff Phrases**
   - Remove sentences that don't add information
   - Avoid concluding paragraphs that just say "this is useful"
   - Don't add generic statements about quality or value
   - Delete phrases like "comprehensive framework", "elegant solution", etc.

4. **Be Specific and Concrete**
   - Instead of "improved performance", describe the specific optimization technique
   - Instead of "enhanced error reporting", specify exactly what information is included in errors
   - Replace "powerful features" with a specific list of capabilities

## Development Practices

### Planning Changes

1. **Document Before Implementing**
   - Always document the steps you plan to take BEFORE making any code changes
   - Break down complex changes into clearly defined smaller steps
   - For each step, explain:
     - What will be changed
     - Why the change is needed
     - How the change relates to the larger goal
     - What tests will verify the change
   - Review your plan for completeness before starting implementation
   - Document any deviations from the plan that occur during implementation

2. **Use Step-by-Step Implementation**
   - After documenting your plan, implement one step at a time
   - Run the full test suite (`sbt rootJVM/test`) after each step
   - Commit logical units of work with clear messages
   - Do not proceed to the next step until the current step passes all tests

### Making Changes

1. **Keep Changes Small and Focused**
   - Make one logical change at a time
   - Break down large changes into smaller, independent steps
   - Each change should be easily reviewable and testable

2. **Testing Requirements**
   - **ALWAYS use the following commands for running tests:**
     ```bash
     # Run all tests from the root project
     sbt rootJVM/test

     # Run a specific test class from the root project
     sbt "rootJVM/testOnly chester.elab.ElabHoleTest"
     
     # You can also run tests for specific modules when needed
     sbt reader/test
     sbt semantics/test
     
     # Run specific test classes in modules
     sbt "reader/testOnly chester.reader.FileParserTest"
     sbt "semantics/testOnly chester.elab.ElabLiteralAndListTest"
     ```
   - **DO NOT** navigate into subdirectories to run tests (e.g., `cd reader && sbt test`)
   - **ALWAYS** run tests from the root project directory
   - ⚠️ **CRITICAL: NEVER use the `-z` test filter option** ⚠️
     - `-z` is NOT the correct syntax for filtering tests in MUnit
     - This option is broken and produces unreliable results
     - Tests may appear to pass when they actually fail
     - This can lead to false confidence in your changes
     - ScalaTest uses `-z` for filtering, but MUnit uses `--tests=` instead
   - ⚠️ **IMPORTANT: Only use correct MUnit filter syntax with `--`** ⚠️
     - When filtering tests, always use proper MUnit syntax:
     - **CORRECT MUnit syntax examples:**
       ```bash
       # Filter by test name
       sbt "rootJVM/testOnly -- --tests=myTestName"
       
       # Filter by glob pattern
       sbt "rootJVM/testOnly -- *MyTest"
       ```
     - **INCORRECT syntax from other frameworks (DO NOT USE):**
       ```bash
       # ScalaTest style (WRONG with MUnit)
       sbt "rootJVM/testOnly -- -t MyTest"
       
       # JUnit style (WRONG with MUnit)
       sbt "rootJVM/testOnly -- -n MyTest"
       
       # Custom incorrect style
       sbt "rootJVM/testOnly -- -only testname"
       ```
   - ALWAYS run `sbt rootJVM/test` before committing changes
   - Fix any test failures before committing
   - Add new tests for new functionality
   - Update existing tests when modifying behavior
   - Test both success and failure cases
   - **💡 Development Tip:** For quickly adding and testing new elaboration scenarios during development, you can add test cases to existing test files in the `semantics/shared/src/test/scala/chester/elab/` directory or create new test files following the existing patterns. To run specific elaboration tests for rapid feedback, use commands like:
     ```bash
     sbt "semantics/testOnly chester.elab.ElabLiteralAndListTest" | cat
     sbt "semantics/testOnly chester.elab.ElabHoleTest" | cat
     ```
     This approach targets elaboration tests directly within the semantics module, providing a faster feedback loop than running the full suite via `sbt rootJVM/test`. Remember to use `sbt rootJVM/test | cat` for final verification before committing.
   - For parser changes:
     - Many tests now run against both old and new readers (V1 and V2)
     - Some complex tests currently only run against V1 (original reader)
     - When adding new parser tests:
       - Use `parseAndCheckBoth` by default for new tests
       - Only use `parseAndCheck` if testing V1-specific features
       - Document if test is V1-only and why
       - Plan to migrate V1-only tests to V2 when ready
     - Test function usage:
       - `parseAndCheck`: V1 parser only
       - `parseAndCheckBoth`: Both V1 and V2 parsers
       - `parseAndCheckV1`: Deprecated alias for parseAndCheckBoth
     - Recently migrated tests:
       - Basic operator sequence tests
       - Pattern matching tests with uniform symbol treatment
       - Simple expression tests
       - Function call tests
       - Dot notation tests
       - Object tests
       - Tuple tests
       - Vararg tests
       - Floating-point number parsing tests
       - List tests with mixed types
     - Tests still needing migration:
       - Complex operator sequences (prefix, mixfix)
       - Telescope parsing
       - Error handling
       - Source position tracking
   - For type checking changes:
     - Test term preservation in elaborated results
     - Test type-level computation works correctly
     - Test error reporting is accurate
     - Test edge cases and corner cases

3. **Verify Changes with Git**
   ```bash
   # After each change - ALWAYS use | cat to prevent terminal control issues:
   git diff | cat            # Review what changed 
   git add <files>          # Stage specific files
   git status | cat         # Verify staged changes 
   git commit -m "..."     # Commit with clear message
   ```
   
   ⚠️ **Always append `| cat` to git diff commands to avoid paging issues.**

4. **Change Verification Checklist**
   - [ ] Changes are minimal and focused
   - [ ] Git diff shows only intended changes
   - [ ] Tests pass after changes
   - [ ] Changes align with existing code style
   - [ ] Review the git diff output carefully
     ```bash
     # Before committing, ALWAYS verify changes with:
     git diff | cat     
     ```
     
     > 💡 **WHY THIS MATTERS**: Failure to review diffs properly is the #1 cause of accidental code deletions and introduction of subtle bugs.
   - [ ] Reviewing git diff output is essential for catching:
     - Accidental deletions of important methods or logic
     - Unintended modification of critical code
     - Formatting changes that might impact behavior
     - Changes to files you didn't intend to modify
   - [ ] Pay special attention to large diffs that might hide important changes
   - [ ] Verify no unrelated changes were included
   - [ ] When making multiple changes, review each file's diff separately for clarity

5. **Post-Commit Verification**
   - **⚠️ MANDATORY**: Always verify your changes after committing with `git diff HEAD^ HEAD | cat`
   - Check the diff output carefully to ensure:
     - No unintended changes were included
     - All intended changes were properly committed
     - File renames and deletions are correctly reflected
     - No sensitive or debug code was accidentally committed
     - No accidental deletions of important logic
   - Verify the commit message accurately describes the changes
   - For complex changes involving multiple files, check each file's changes individually

6. **Git Command Tips**
   - Always use `| cat` with git commands that might trigger paging:
     ```bash
     git diff | cat
     git log | cat
     git show | cat
     ```
   - This ensures consistent output and avoids interactive paging

### Terminal Control with Git Commands

1. **⚠️ CRITICAL: ALWAYS Use `| cat` Suffix**
   - Git commands that might trigger paging or interactive prompts MUST ALWAYS end with `| cat`
   - This is a MANDATORY practice, not a suggestion
   - This ensures consistent output and prevents terminal control issues
   - Failure to use `| cat` is the leading cause of incomplete reviews and missed errors
   - Examples:
     ```bash
     git checkout main | cat
     git merge --no-ff branch | cat
     git log | cat
     git diff | cat
     git show | cat
     git branch | cat
     ```

2. **Common Git Operations**
   ```bash
   # Switching branches
   git checkout main | cat
   git checkout -b new-branch | cat

   # Merging
   git merge --no-ff feature-branch | cat
   git merge --abort | cat  # If merge conflicts occur

   # Viewing changes
   git status | cat
   git log --oneline | cat
   git show HEAD | cat

   # Committing
   git add . | cat
   git commit -m "type: description" | cat
   ```

3. **Why This Matters**
   - Prevents terminal from entering interactive mode
   - Ensures consistent output formatting
   - Avoids getting stuck in pagers like `less`
   - Makes automation and scripting more reliable

### Troubleshooting Development Issues

1. **Recovering from Broken Edit Tools**
   - If edit tools in your IDE or development environment are broken/malfunctioning, you can use git to recover:
     ```bash
     # Discard changes to a specific file
     git checkout -- path/to/file | cat

     # Discard all changes in the working directory
     git checkout -- . | cat

     # Revert to a specific commit
     git checkout [commit-hash] -- path/to/file | cat
     ```
   - This approach is especially useful when tools that normally handle editing break unexpectedly
   - Always verify what you're checking out before executing the command to avoid losing important changes

### AI Agent Testing Instructions

1. **Terminal Interruption Issues**
   - If you are an AI agent working on Chester code and notice:
     - Frequent `^C` characters appearing in command output
     - Commands being interrupted prematurely
     - Test results not displaying properly
     - Terminal output being cut off
   - STOP attempting to run tests and:
     - Inform the user about the terminal connection issues
     - Ask the user to run the tests manually
     - Request that the user provide the test results
     - This indicates a problem with the terminal connection, not with the code itself

2. **Test Running Best Practices for AI Agents**
   - **ALWAYS use these exact commands for running tests:**
     ```bash
     # Run all tests
     sbt rootJVM/test | cat

     # Run a specific test class (include quotation marks)
     sbt "semantics/testOnly chester.elab.ElabLiteralAndListTest" | cat
     ```
   - **NEVER** attempt to run tests with other project paths like `cli/test`, `semantics/test`, etc.
   - ⚠️ **CRITICAL: NEVER use the `-z` test filter option** ⚠️
     - Example of what NOT to do: `sbt "semantics/testOnly chester.elab.ElabLiteralAndListTest -z myTest"`
     - The `-z` flag is completely broken and will cause misleading results
     - Tests might appear to pass when they should fail
     - Using `-z` will lead to incorrect conclusions about code behavior
   - ⚠️ **CRITICAL: NEVER use `--` to pass arguments to tests** ⚠️
     - Example of what NOT to do: `sbt "rootJVM/testOnly -- -t MyTest"`
     - This will cause tests to run incorrectly or not at all
     - No arguments should be passed after the test class name
   - Always run full test suites rather than individual tests when possible
   - Verify that terminal commands execute completely before proceeding
   - If a test command produces an error about not finding the test class:
     - First try the full `rootJVM/test` command to run all tests
     - Then check if the test class path is correct
     - Do not experiment with different project paths
   - If tests are taking too long to complete, inform the user and suggest they run the tests locally

## Term System Architecture

Chester uses a unified term representation architecture to support multiple platforms:

### Term Definition Structure

1. **Unified Term Definition**
   - All term types are defined in a single file: `syntax/shared/src/main/scala/chester/syntax/core/Term.scala`
   - This approach simplifies the codebase and eliminates the need for separate platform-specific implementations
   - Each term type follows a consistent pattern with standard methods and field annotations

### Import Guidelines

1. **DO** use `import chester.syntax.core.*`
   - This will give you access to all term implementations

```scala
// CORRECT
import chester.syntax.core.*

// INCORRECT - unnecessarily specific imports
import chester.syntax.core.BlockTerm
import chester.syntax.core.FCallTerm
```

### Pattern Matching and Type Usage

Use concrete term types directly for pattern matching:

```scala
// CORRECT
case t: BlockTerm => {
  val reducedStatements = t.statements.map(stmt => r.reduce(stmt))
  val reducedResult = r.reduce(t.result)
  BlockTerm(reducedStatements, reducedResult, t.meta)
}
```

### Term Type Implementation Pattern

All term types follow a consistent implementation pattern:

```scala
case class ExampleTerm(
  @child var field1: Term,        // Use @child for term fields that should be traversed
  @const val field2: String,      // Use @const for non-term fields 
  @const meta: OptionTermMeta
) extends BaseTerm {
  override type ThisTree = ExampleTerm
  
  // Pretty printing method
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("ExampleTerm(") <> field1.toDoc <> Doc.text(")")
  
  // Tree traversal method
  override def descent(f: Term => Term, g: TreeMap[Term]): Term =
    thisOr(copy(field1 = g(field1)))
}
```

### Adding New Term Types

When adding a new term type:

1. Add it directly to `Term.scala`
2. Follow the existing pattern for similar term types
3. Implement all required methods (`toDoc`, `descent`, etc.)
4. Use correct field annotations (`@child`, `@const`)
5. Extend the appropriate base type (e.g., `TypeTerm`, `ExprTerm`)

### Example: Adding a New Term Type

For example, to add a new term type for union types:

```scala
case class UnionTypeTerm(
  @child var types: Vector[Term],
  @const meta: OptionTermMeta
) extends TypeTerm {
  override type ThisTree = UnionTypeTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("UnionType(") <> Doc.join(Doc.text(", "), types.map(_.toDoc)) <> Doc.text(")")
  override def descent(f: Term => Term, g: TreeMap[Term]): Term =
    thisOr(copy(types = types.map(g)))
}
```

### Key Term Types

The system includes several important term categories:

1. **Expression Terms**: Represent runtime values (variables, function calls, literals)
2. **Type Terms**: Represent type information (primitive types, function types, union types)
3. **Statement Terms**: Represent declarations and control flow (let/def bindings, trait definitions)
4. **Pattern Terms**: Represent pattern matching constructs
5. **Special Terms**: Represent special language constructs (holes, placeholders)

Each category has a base trait that defines its common behavior.

### Why This Matters

- **Simplified Architecture**: The unified term definition makes the codebase more maintainable
- **Cross-Platform Compatibility**: All platforms use the same term representation
- **Consistent Patterns**: All term types follow the same implementation pattern
- **Easier Extensions**: Adding new term types follows a clear and consistent approach

## Elaboration and Reduction Strategy

### Reduction During Type Checking

1. **Keep Original Forms**
   - The elaborator MUST preserve original terms in the elaborated result
   - NEVER reduce during elaboration
   - Only use reduction internally during type checking when absolutely necessary
   - This makes the elaborated code identical to source code, making it:
     - Easier to debug
     - Easier to understand
     - Better for error messages
     - More suitable for further transformations

2. **When to Reduce**
   - Only TWO places should use reduction:
     1. Type equality checking in unification
     2. Field access checking on type-level terms
   - Use `ReduceMode.TypeLevel` for these internal reductions
   - NEVER use reduction in elaborated results

Example:
```scala
// Original code
def idType(x: Type): Type = x;
let aT = idType(A);
def getA(x: aT): Integer = x.a;

// WRONG - reducing during elaboration:
LetStmtTerm(localv, reducer.reduce(idType(A)), ty, meta)

// RIGHT - keeping original term:
LetStmtTerm(localv, idType(A), ty, meta)

// RIGHT - internal reduction only for field checking:
def checkFieldAccess(recordTy: Term, field: Name): Term = {
  // Use type-level reduction only for checking field existence
  // Keep original term in result
  // ...
}
```

### Reduction Context and Type Checking

1. **Reduction Context Setup**
   - Each `Context` instance provides its own reduction context via `toReduceContext`
   - This ensures consistent reduction behavior during type checking
   - Allows for future extensions to reduction context

2. **Type-Level Reduction**
   - Only reduce type-level terms when necessary for type checking
   - Keep original terms in elaborated results
   - Use `ReduceMode.TypeLevel` to control reduction behavior

3. **Field Access Checking**
   - Use type-level reduction to verify field existence
   - Keep original terms in field access expressions
   - Report errors using original terms for better error messages

### Common Pitfalls

1. **Over-reduction**
   - Don't reduce terms during elaboration
   - Don't reduce terms when adding to context
   - Only reduce when needed for type checking

2. **Loss of Original Terms**
   - Always preserve original terms in elaborated results
   - Don't reflect internal reductions in output
   - Keep source code structure intact

3. **Incorrect Reduction Context**
   - Always use proper reduction context from current context
   - Don't create new reduction contexts unnecessarily
   - Use consistent reduction mode for type checking

## Coding Conventions

### Imports

- **Document Utilities:** When using utilities from the `chester.utils.doc` package (such as `Doc`, `PrettierOptions`, or extension methods like `render`), prefer using a single wildcard import: `import chester.utils.doc.*`.

### String Formatting and Internationalization

1. **Use Template Strings for User-Facing Text**
   - ALWAYS use template strings (`t""`) for user-facing messages, not string interpolation (`s""`)
   - ALWAYS use template strings (`t""`) for plain user-facing text, even without variables
   - Always import the internationalization package: `import chester.i18n.*`
   - This ensures proper internationalization and localization support
   ```scala
   // CORRECT - using template strings for user-facing text
   import chester.i18n.*
   
   val username = "Alice"
   val message = t"Hello $username, welcome to Chester!"
   
   // CORRECT - using template strings for plain text without variables
   val errorMessage = t"Operation failed. Please try again."
   
   // INCORRECT - using string interpolation for user-facing text
   val message = s"Hello $username, welcome to Chester!"
   
   // INCORRECT - using regular string literals for user-facing text
   val errorMessage = "Operation failed. Please try again."
   ```

2. **String Interpolation for Internal Use Only**
   - Only use string interpolation (`s""`) for internal, non-user-facing strings
   - Examples include debug logging, internal identifiers, and non-displayed text
   ```scala
   // CORRECT - using string interpolation for internal/technical content
   val logMessage = s"DEBUG: Processing request from $username with params $params"
   val technicalId = s"${prefix}_${uuid}"
   ```

3. **Why This Matters**
   - Template strings enable automatic translation and localization
   - They maintain consistent messaging across the application
   - They allow for future language additions without code changes
   - They ensure a better experience for non-English users

### Core Principles

1. **Use C-style Braces**
   - Always use braces for control structures, even for single-line blocks
   - Opening brace on the same line
   - Closing brace on its own line
   ```scala
   // Good
   if (condition) {
     doSomething()
   } else {
     doSomethingElse()
   }

   // Bad - No braces
   if (condition)
     doSomething()

   // Bad - Indentation-based syntax
   if (condition)
     doSomething()
     andThenThis()  // Unclear scope
   ```

2. **No Indentation-Based Syntax**
   - Do not rely on indentation for scope
   - Always use explicit braces to define scope
   ```scala
   // Good
   def method() = {
     val result = {
       val x = compute()
       transform(x)
     }
     result
   }

   // Bad - Indentation-based
   def method() =
     val result =
       val x = compute()
       transform(x)
     result
   ```

3. **Function Definitions**
   - Opening brace on the same line as the function definition
   - Use explicit return types
   ```scala
   // Good
   def process(input: String): Result = {
     // implementation
   }

   // Bad
   def process(input: String): Result =
     // implementation
   ```

4. **Pattern Matching**
   - Use braces for case blocks
   - Align case statements
   ```scala
   // Good
   expr match {
     case Literal(value) => {
       process(value)
     }
     case Identifier(name) => {
       lookup(name)
     }
   }

   // Bad
   expr match
     case Literal(value) =>
       process(value)
     case Identifier(name) =>
       lookup(name)
   ```

5. **For Comprehensions**
   - Use braces instead of indentation
   ```scala
   // Good
   for {
     x <- xs
     y <- ys
   } yield {
     combine(x, y)
   }

   // Bad
   for
     x <- xs
     y <- ys
   yield combine(x, y)
   ```

### Additional Guidelines

- Use parentheses for method calls even when they could be omitted
- Prefer multi-line formatting with braces for complex expressions
- Use explicit type annotations for public APIs
- Keep line length reasonable (max 120 characters)
- Use two-space indentation within braces

### Enum Usage

1. **Prefer Enums Over String Literals**
   - Use enums for representing categories, types, or states
   - Never use string literals as pseudo-enums
   ```scala
   // Good
   enum DebugCategory {
     case Cell
     case Tyck
     case Reducer
   }

   // Bad
   val category = "CELL" // Using string literals as enum values
   ```

2. **Enum Naming Conventions**
   - Use PascalCase for enum type names
   - Use PascalCase for enum values
   - Keep enum names clear and descriptive

3. **Enum Usage**
   - Import enum values when needed
   - Use qualified access for clarity in other contexts
   - Use pattern matching for exhaustive handling
   ```scala
   // Good usage
   import DebugCategory.*

   val category = Cell

   category match {
     case Cell => handleCell()
     case Tyck => handleTyck()
     case Reducer => handleReducer()
   }
   ```

## Debugging Practices

1. **Understand Before Fixing**
   - Always understand the root cause of an issue before attempting to fix it
   - Use the Debug utility with appropriate categories to trace program execution
   - Analyze call stacks to identify where issues occur
   - Create minimal test cases that reproduce the issue

2. **Systematic Debugging Process**
   - Enable relevant debug logging (`Debug.enable(DebugCategory.XXX)`)
   - Add strategic logging points to track object state and method execution
   - Verify assumptions about code behavior using logs and assertions
   - Isolate the issue by creating focused test cases
   - Document your findings to help others understand the problem

3. **Debug-First Approach**
   - When facing complex issues, prioritize debugging over immediate fixes
   - Add temporary debugging code when needed, but mark it clearly and remove when done
   - Consider adding permanent debugging hooks for areas prone to issues
   - Document debugging insights even if they seem obvious
