# AI Agents Guide for Scala 2 Compiler

This document provides guidance for AI agents working with the Scala 2.13.x compiler codebase.

## Overview

This repository contains the Scala 2 standard library, compiler, and language specification. For Scala 3, see [scala/scala3](https://github.com/scala/scala3).

**Key Facts:**
- **Language:** Scala 2.13.x (maintenance mode, 2.12.x under minimal maintenance)
- **Build Tool:** sbt
- **Test Frameworks:** JUnit, ScalaCheck, Partest
- **CI:** GitHub Actions, Jenkins on scala-ci.typesafe.com
- **Issue Tracking:** [scala/bug](https://github.com/scala/bug)

## Repository Structure

```
scala/
├── src/
│   ├── library/          # Scala Standard Library
│   ├── reflect/          # Scala Reflection
│   ├── compiler/         # Scala Compiler
│   ├── repl/             # REPL core
│   ├── scaladoc/         # Documentation tool
│   └── [other modules]
├── test/
│   ├── files/            # Partest tests (pos, neg, run, jvm)
│   ├── junit/            # JUnit tests
│   └── scalacheck/       # ScalaCheck tests
├── spec/                 # Language specification (markdown)
├── build.sbt             # Main sbt build
└── project/              # sbt build configuration
```

## Common Tasks

### Building & Testing

```bash
# Start sbt session
sbt

# Compile all sub-projects and generate runner scripts (in sbt)
dist/mkQuick

# Run REPL/compiler (in shell, not in sbt)
build/quick/bin/scala / build/quick/bin/scalac

# Run JUnit tests (in sbt)
junit/test
junit/testOnly *Foo

# Run ScalaCheck tests (in sbt)
scalacheck/test

# Run partest tests (in sbt)
partest test/files/neg/delayed-init-ref.scala
partest --neg                    # all neg tests
partest --grep range             # tests matching pattern
partest --failed                 # rerun failed tests
partest --update-check           # update .check files
```

### Bootstrapping

When modifying code generation, bootstrap to see changes in library/compiler bytecode:

```bash
sbt> restarrFull    # Build and publish locally, switch to new version
sbt> reload         # Revert to STARR version
```

## Testing Guide

### Test Categories

1. **JUnit** (`test/junit/`): Unit tests, compiled with STARR, run against `quick` library
   - Use for library functionality
   - Use `BytecodeTesting` to invoke compiler programmatically

2. **ScalaCheck** (`test/scalacheck/`): Property-based tests
   - For testing with random data
   - Example: `test/scalacheck/range.scala`

3. **Partest** (`test/files/`): Integration tests, compiled/run with bootstrapped `quick` compiler
   - `pos/`: Must compile successfully
   - `neg/`: Must fail compilation, output matches `.check` file
   - `run/`: Compiles and runs `Test.main`, output matches `.check`
   - `jvm/`: Same as `run`

Use partest for compiler work, JUnit/ScalaCheck for library work.

### Partest Tips

- **Flags:** Add `//> using options -Werror -Xlint` at top of source
- **Java flags:** `// javac: <flags>`
- **Filters:** `// filter: <regex>` to exclude output lines
- **Version constraints:** `// javaVersion 8` or `15+` or `9 - 11`
- **Separate compilation:** Use `_N` suffixes (e.g., `A.scala`, `B_1.scala`, `Test_2.scala`)

### Creating Check Files

In sbt:
- `partest --update-check test/files/neg/my-test.scala`

## Code Standards

### DRY Principle
- Don't repeat yourself
- Abstract common patterns
- Avoid copy-paste

### Boy Scout Rule
- Leave code better than you found it
- Opportunistic refactoring within scope

### Clean Commits
- Active, present tense commit messages
- Subject line ≤ 72 characters
- End PR description (but not commit message) with "Fixes scala/bug#NNNN"
- Don't @mention in commit messages (use PR comments)

### Commit History
- Every commit must pass CI (for `git bisect`)
- Amend and force-push for review feedback
- Keep history clean and linear

## Benchmarking

Location: `test/benchmarks/`
Uses: [sbt-jmh plugin](https://github.com/ktoso/sbt-jmh)

```bash
# Run benchmark
bench/Jmh/run scala.collection.mutable.ListBufferBenchmark

# Run custom runner
bench/Jmh/runMain scala.collection.mutable.OpenHashMapRunner

# Useful JVM options
-jvmArgs -XX:+PrintCompilation      # JIT compilation events
-jvmArgs -verbose:gc                # GC events
-jvmArgs -XX:+PrintInlining         # Inlining decisions
-jvmArgs -XX:+PrintAssembly         # Disassembled code (requires hsdis)
```

## AI Agent Considerations

### When Working With This Codebase

1. **Read First:**
   - Check [scala/bug](https://github.com/scala/bug) for related issues
   - Review existing tests for similar functionality
   - Understand binary/source compatibility implications

2. **Test Strategy:**
   - Library changes → JUnit or ScalaCheck
   - Compiler changes → Partest or JUnit with BytecodeTesting
   - Include tests in the same commit as the fix
   - Explain if testing is infeasible

3. **Compatibility Checks:**
   - Run MiMa to verify binary compatibility (`library/mimaReportBinaryIssues` in sbt)
   - Consider impact on type inference
   - Test with community build if available

4. **Documentation:**
   - Update Scaladoc for API changes
   - Update spec/ for language changes
   - Update package-level docs if appropriate

### Common Pitfalls to Avoid

- ❌ Adding public APIs to standard library (use scala-library-next)
- ❌ Breaking binary compatibility without careful review
- ❌ Changing type inference without team discussion
- ❌ Not running partest for compiler changes
