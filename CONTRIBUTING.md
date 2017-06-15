# Contributing

## Getting started

You need the following:

- Git
- JDK 8
- SBT

### Workflow

- Make sure you have signed the [Scala CLA](http://www.lightbend.com/contribute/cla/scala).
- You should perform your work in its own Git branch.
- Then open a pull request on GitHub, with `master` as base branch.

### Build

- Compile the collections and run the tests:
  
  ~~~
  >; compile; test
  ~~~
- Run the memory benchmark:
  
  ~~~
  > memoryBenchmark/charts
  ~~~
- Run the execution time benchmark:
  
  ~~~
  > timeBenchmark/charts
  ~~~
  - Charts are produced as .png files in the `benchmarks/time/target/` directory.
    Each `@Benchmark` method produces a .png chart with the same name (e.g. the
    `foreach` benchmark produces a `foreach.png` chart). In each chart, we
    aggregate results from all the benchmark classes that have a benchmark with
    the same name (e.g. the `foreach.png` chart aggregates information from the
    `ListBenchmark`’s `foreach` method, `LazyListBenchmark`’s `foreach` method,
    etc.).
  - Running the whole benchmark suite takes time (several hours) and produces
    charts containing series for each collection type. You can restrict the
    benchmarks to be run by JMH to get more readable results. For instance, to
    run only benchmarks whose name contain `Array`:
    
    ~~~
    > timeBenchmark/charts Array
    ~~~

## Roadmap

1. Q2 2017: release a v0.2.0 targeting Scala 2.13 and Dotty.
    - Implement most of the current collections types
    - Implement most of the current collections operations
    - Alternative to `CanBuildFrom` to get implicit builders
    - Include tests for correctness (taken from the current collections
      and from scala-collections-laws)
    - Provide a rewriting tool that migrates a code base from the current
      collections to the strawman
2. Q3 2017: add features and tune for performance
    - Decisions will be made on the basis of benchmarks
    - Add Scala.js support
    - Consider the inclusion of new collection types (such as `Spandex`, `Steque`
      or `ArrayDeque`)
    - Consider the introduction of new operations (such as database-like joins,
      variants of groupBy, etc.)
    - Java interoperability
    - Separate project for parallel collections
3. Q4 2017: move to `scala/scala` repository
