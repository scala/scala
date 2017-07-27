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
