# Contributing

Want to be part of this project but don’t know what you can do to help? You should have a look at the
[low hanging fruit](https://github.com/scala/collection-strawman/issues?q=is%3Aissue+is%3Aopen+label%3A%22low+hanging+fruit%22)
issues!

## Getting started

You need the following:

- Git
- JDK 8
- SBT

### Workflow

- Make sure you have signed the [Scala CLA](http://www.lightbend.com/contribute/cla/scala).
- You should perform your work in its own Git branch.
- Then open a pull request on GitHub, with `master` as base branch.

Have a look at the [Waffle.io board](https://waffle.io/scala/collection-strawman) to
quickly know which issues are ready and which are already in progress.

### Build

- Compile the collections and run the tests:
  
  ~~~
  >; compile; test; junit/test
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

## Migration tool

Several levels of contribution are possible!

### Report a missing case

Create an issue tagged with the
[migration](https://github.com/scala/collection-strawman/labels/migration) label.
Embrace `diff`s to describe differences between the standard collections and
the strawman:

~~~ diff
- xs.toIterator
+ xs.iterator()
~~~

### Add a missing test case

Even better, instead of providing a diff, you can directly add it as a test case!

1. Fork this repository and create a separate branch;

2. Add a file in the `scalafix/input/src/main/scala/fix/` directory with code
   that uses the standard collections:
   
~~~ scala
class toIteratorVsIterator(xs: Iterable[Int]) {
  xs.toIterator
}
~~~

3. Add a corresponding file in the `scalafix/output/src/main/scala/fix/` directory
   with the same code but using the strawman:
   
~~~ scala
import strawman.collection.Iterable

class toIteratorVsIterator(xs: Iterable[Int]) {
  xs.iterator()
}
~~~

4. Check that your code example compiles
    - locally publish the strawman by running `sbt publishLocal` from the
      project root directory,
    - run sbt from the `scalafix/` directory
      and then run the following tasks `; input/compile ; output/compile`;

5. Commit your changes, push your branch to your fork and create a pull request.

Then maybe someone will take over and implement your use case… or maybe you will
(see next section)!

### Implement a missing case

Even better, complete the migration tool implementation to support the missing case!

After you have added the missing case (see previous section), run the following
sbt task (with sbt started from the `scalafix/` directory) to run the
migration tool on the input files and check whether the result matches the
expected output files:

~~~
> tests/test
~~~

Fix the implementation of the rewrite (in the
`rewrites/src/main/scala/fix/Collectionstrawman_v0.scala` file) until the
tests are green. You can find more help about the scalafix API in its
[documentation](https://scalacenter.github.io/scalafix/#Creatingyourownrewrite).
