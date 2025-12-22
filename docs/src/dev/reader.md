# Chester Reader Architecture

Design of Chester's parsers ("readers") that transform source code into abstract syntax trees.

## Overview

Chester currently has two parser implementations:

1. **ReaderV1**: The original parser using FastParse combinators
2. **ReaderV2**: The newer implementation using a token-based state machine

Both parsers produce semantically identical ASTs using different internal approaches.

## Core Design Principles

1. **Context-Free Parsing**: Uniform rules for all expressions; identifiers treated consistently
2. **Separation of Concerns**: Parse syntax without imposing semantics 
3. **Uniform Symbol Treatment**: No special keywords - just identifiers and operators
4. **Flat Operator Sequences**: Operator precedence handled later in the semantic phase
5. **Newline Significance**: `}\n` terminates expressions in blocks
6. **Block Return Values**: Last expression in a block is its return value

## ReaderV1 Implementation

ReaderV1 uses the FastParse library to implement a parser combinator approach.

### Key Components

- **TokenParsers**: Small parsers for basic lexemes (identifiers, literals, operators)
- **Combinators**: Composable functions that build larger parsers from smaller ones
- **ParsingContext**: Tracks parsing state (e.g., whether currently in an operator sequence)
- **ExprMeta**: Metadata handling for source positions and comments

### Characteristics

- Declarative grammar definitions
- FastParse-based error reporting
- Recursive descent parsing model

### Implementation Structure

ReaderV1 consists of:

1. **Expression Parsers**: Methods like `parseExpr`, `parseAtom`, and `parseOperator` form the core of the parser. They use FastParse combinators to build complex parsers from simpler ones.

2. **Context Tracking**: A `ParsingContext` object tracks the current parsing state, including whether we're in an operator sequence, a block, or other specialized contexts.

3. **Source Position Tracking**: Dedicated methods map character positions to line/column positions for error reporting, with special handling for UTF-16 surrogate pairs.

4. **Whitespace and Comment Handling**: Dedicated parsers for whitespace, line endings, and comments ensure these elements are preserved in the AST.

5. **Parser Extensions**: Custom extension methods for FastParse parsers add support for metadata attachment, relaxed parsing, and error recovery.

6. **Parser Composition**: The implementation composes smaller parsers into larger ones, following FastParse's combinator approach.

## ReaderV2 Implementation

ReaderV2 uses a custom tokenizer and a state machine-based approach for parsing, with significant improvements to block termination detection and object expression parsing.

### Key Components

- **Lexer**: Converts source code into a stream of tokens for efficient parsing
- **ReaderState**: Tracks current token position, history, and pending whitespace/comments
- **ReaderContext**: Contains context flags like `newLineAfterBlockMeansEnds` for parsing decisions
- **Token**: Represents tokens like identifiers, operators, literals, with source position information
- **Token Handlers**: Specialized methods for parsing different token types and structures

### Characteristics

- Pre-tokenization for efficient token stream processing
- Separate lexing and parsing phases for cleaner code organization
- Context-aware parsing with explicit state tracking
- Enhanced UTF-16 aware Unicode and emoji handling
- Robust block termination detection with the `}\n` pattern
- Comprehensive object expression support with multiple key types
- Optimized comment handling and attachment

### Implementation Structure

ReaderV2 consists of:

1. **Two-Phase Parsing**: Separates tokenization from parsing, with a dedicated Tokenizer creating a stream of tokens before parsing begins.

2. **State Management**: The parser maintains state through two complementary objects:
   - **ReaderState**: Tracks token position, history, and pending whitespace/comments
   - **ReaderContext**: Contains context flags like `newLineAfterBlockMeansEnds` for syntactic decisions 
   - Together they enable precise tracking of parser state and contextual information

3. **Context-Aware Processing**: Context flags enable important syntactic decisions like proper block termination with the `}\n` pattern, while maintaining uniform symbol treatment.

4. **Optimized Comment Handling**: Non-recursive methods like `skipComments()` and `pullComments()` efficiently manage comment attachment, replacing the previous recursive approach.

5. **Robust Block Termination**: The special `}\n` pattern detection is implemented in the `checkForRBraceNewlinePattern()` method, which uses the `newLineAfterBlockMeansEnds` flag from ReaderContext to determine when blocks should end.

6. **Enhanced Object Expressions**: Support for multiple key types:
   - Identifier keys (e.g., `{ x = 1 }`)
   - String literal keys (e.g., `{ "x" = 1 }`)
   - Symbol literal keys (e.g., `{ 'x = 1 }`)
   - Both `=` and `=>` operators in object clauses

7. **Error Handling**: The parser produces structured `ParseError` objects with detailed source position information and recovery mechanisms.

8. **Bottom-Up Construction**: Parsing builds expressions from atoms and then extends them through continuation-based parsing in `parseRest()`.

## Key Similarities Between Implementations

Both parsers:

1. Track source positions for error reporting
2. Preserve comments in the AST
3. Handle the `}\n` block termination pattern
4. Produce flat operator sequences without precedence handling
5. Parse the same language syntax
6. Use context tracking for parsing decisions
7. Generate identical AST structures

## Key Differences Between Implementations

| Feature | ReaderV1 | ReaderV2 |
|---------|----------|----------|
| **Parsing Approach** | Parser combinators (FastParse) | Token-based state machine |
| **Error Recovery** | Limited | Enhanced with token-based recovery |
| **Token Creation** | On-demand during parsing | Separate tokenization phase |
| **State Handling** | Implicit in parse context | Explicit in ReaderState |
| **Code Structure** | Grammar-centric | Process-centric |
| **Performance** | Good | Better (especially on large files) |
| **Unicode Support** | Basic | Enhanced with better UTF-16 handling |

## Testing Infrastructure

Chester's test framework validates parser correctness and compatibility between V1 and V2 implementations. This framework, defined in `reader/shared/src/test/scala/chester/reader/parseAndCheck.scala`, provides several key testing functions:

### Core Testing Functions

1. **Parser-Specific Testing**:
   - `parseV1(input)`: Parses input with V1 parser only and returns the result
   - `parseV2(input)`: Parses input with V2 parser only and returns the result
   - `parseAndCheckV1(input, expected)`: Tests V1 parser against expected output
   - `parseAndCheckV2(input, expected)`: Tests V2 parser against expected output

2. **Cross-Parser Verification**:
   - `parseAndCheckBoth(input, expected)`: Tests both parsers and ensures they produce identical results
   - Tests backward compatibility and feature parity

3. **Top-Level Parsing**:
   - `parseTopLevelV1/V2` and `parseAndCheckTopLevelV1/V2/Both`: Similar functions for testing top-level parsing
   - Handle file-level parsing with multiple expressions

### Error Reporting

The testing framework provides error reporting with:

- Detailed error messages showing exact failure position
- Visual pointer to error location in source code
- Context-aware error descriptions
- Comparison between expected and actual AST structures

### Serialization Verification

The framework also tests that parsed expressions can be correctly serialized and deserialized:

- Verifies JSON serialization with `read[Expr](write[Expr](value))`
- Confirms binary serialization with `readBinary[Expr](writeBinary[Expr](value))`
- Ensures AST structures maintain integrity through serialization cycles

### Test Organization

Parser tests are organized into several categories:

1. **Expression Tests**: Verify parsing of individual expression types
2. **Integration Tests**: Test combined language features
3. **Regression Tests**: Ensure previously fixed issues don't reoccur
4. **Migration Tests**: Track progress in supporting V1 features in V2

### File-Based Testing

In addition to the core testing functions, Chester implements file-based integration tests:

- **FileParserTest.scala**: Tests ReaderV2 against a suite of test files in `tests/parser` directory
- **FileParserTestV1.scala**: Tests ReaderV1 against the same test suite for comparison

These file-based tests:
- Ensure consistency when parsing complete Chester files
- Verify parser behavior across a wide range of syntax combinations
- Automatically generate expected output for regression testing
- Maintain backward compatibility during parser evolution

## Future Development

ReaderV2 is the focus of ongoing development, with priorities including:

1. Completing error recovery implementation
2. Adding source maps support
3. Migrating any remaining V1-only tests
4. Expanding test coverage
5. Optimizing token handling for better performance

See [devlog.md](devlog.md) for chronological implementation details.
