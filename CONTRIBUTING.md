# Welcome! Thank you for contributing to Scala!

We follow the standard GitHub [fork & pull](https://help.github.com/articles/using-pull-requests/#fork--pull) approach to pull requests. Just fork the official repo, develop in a branch, and submit a PR!

You're always welcome to submit your PR straight away and start the discussion (without reading the rest of this wonderful doc, or the [`README.md`](README.md)). The goal of these notes is to make your experience contributing to Scala as smooth and pleasant as possible. We're happy to guide you through the process once you've submitted your PR.

## The Scala Community

In 2014, you -- the Scala community -- matched the core team at EPFL in number of commits contributed to Scala 2.11, doubling the percentage of commits from outside EPFL/Lightbend since 2.10. Excellent work! (The split was roughly 25/25/50 for you/EPFL/Lightbend.)

We are super happy about this, and are eager to make your experience contributing to Scala productive and satisfying, so that we can keep up this growth. We can't do this alone (nor do we want to)!

This is why we're collecting these notes on how to contribute, and we hope you'll share your experience to improve the process for the next contributor! (Feel free to send a PR for this note, send your thoughts to \#scala-contributors (on [Discord](https://discord.com/invite/scala)) or contributors.scala-lang.org (Discourse).)

By the way, the team at Lightbend is: @lrytz, @retronym, @SethTisue, and @dwijnand.

## What kind of PR are you submitting?

Regardless of the nature of your Pull Request, we have to ask you to digitally sign the [Scala CLA](https://www.lightbend.com/contribute/cla/scala), to protect the OSS nature of the code base.

You don't need to submit separate PRs for 2.12.x and 2.13.x. Any change accepted on 2.12.x will, in time, be merged onto 2.13.x too. (We are no longer accepting PRs for 2.11.x.)

### Documentation

Whether you finally decided you couldn't stand that annoying typo anymore, you fixed the outdated code sample in some comment, or you wrote a nice, comprehensive, overview for an under-documented package, some docs for a class or the specifics about a method, your documentation improvement is very much appreciated, and we will do our best to fast-track it.

You can make these changes directly in your browser in GitHub, or follow the same process as for code. Up to you!

For bigger documentation changes, you may want to poll contributors.scala-lang.org first, to quickly gauge whether others support the direction you're taking, so there won't be any surprises when it comes to reviewing your PR.

### Code

For bigger changes, we do recommend announcing your intentions on contributors.scala-lang.org first, to avoid duplicated effort, or spending a lot of time reworking something we are not able to change at this time in the release cycle, for example.

The kind of code we can accept depends on the life cycle for the release you're targeting. The current maintenance release (2.12.x) cannot break source/binary compatibility, which means public APIs cannot change. It also means we are reluctant to change, e.g., type inference or implicit search, as this can have unforeseen consequences for source compatibility.

#### Bug Fix

At the end of the PR description, which is autofilled with the commit message if there is only one commit, add the phrase, "Fixes scala/bug#NNNN", where `https://github.com/scala/bug/issues/NNNN` tracks the bug you're fixing. Github will turn your bug number into a link.

We also recommend naming your branch after the ticket number.

Please make sure the ticket's milestone corresponds to the upcoming milestone for the branch your PR targets. The CI automation will automatically assign the milestone after you open the PR.

#### Enhancement or New Feature

For longer-running development, likely required for this category of code contributions, we suggest you include `topic/` or `wip/` in your branch name, to indicate that this is work in progress and that others should be prepared to rebase if they branch off your branch.

Any language change (including bug fixes) must be accompanied by the relevant updates to the spec, which lives in the same repository for this reason.

A new language feature or other substantial enough language change requires a SIP (Scala Improvement Process) proposal. For more details on submitting SIPs, see [how to submit a SIP](https://docs.scala-lang.org/sips/sip-submission.html).

## Guidelines

Here is some advice on how to craft a pull request with the best possible
chance of being accepted.

### Tests

Bug fixes should include regression tests -- in the same commit as the fix.

If testing isn't feasible, the commit message should explain why.

New features and enhancements must be supported by a respectable test suite.

Some characteristics of good tests:

* includes comments: what is being tested and why?
* be minimal, deterministic, stable (unaffected by irrelevant changes), easy to understand and review
* have minimal dependencies: a compiler bug test should not depend on, e.g., the Scala library

There are mainly three kinds of tests: unit tests, property-based tests, and integration tests.

#### JUnit

For unit tests we use JUnit, which can be run from sbt shell as follows:

```
root> junit/testQuick
```

It might take a few minutes the first time you run `junit/testQuick`, but from the second time onwards
sbt will only run the tests that is affected by the code change since the last run.
See `test/junit/` for examples of JUnit tests.

JUnit tests will be compiled with the `starr` compiler, and run against the `quick` library. Some JUnit tests (search for `BytecodeTesting`) invoke the compiler programmatically and test its behavior or output, these tests use the `quick` compiler. 

`starr` is the Scala release used to build the compiler and library, usually the last release. `quick` is the result of that compilation. See also ["Build Setup"](https://github.com/scala/scala#build-setup) in the README.

#### ScalaCheck

For testing that can benefit from having lots of randomly generated data, property-based testing should be used.
This is run from sbt shell as follows:

```
root> scalacheck/testOnly ByOneRangeTest
```

See `test/scalacheck/range.scala`.

#### Partest

scala/scala comes with a powerful integration testing tool called Partest.
Using Partest you can compile or run some Scala code, and compare it against the expected output.
In the source tree, partests are located under `test/files/<category>/`. The main categories are:

- `pos`: These files must compile successfully.
- `run`: In addition to compiling, `Test.main` is run and its output is compared against the test's `.check` file.
- `neg`: These files must NOT compile, with compiler output matching the expected output in the `.check` file.
- Other categories such as `jvm` behave the same as `run` category.

To run a single negative test from sbt shell:

```
root> partest --verbose test/files/neg/delayed-init-ref.scala
```

A test can be either a single `.scala` file or a directory containing multiple `.scala` and `.java` files.
For testing separate compilation, files can be grouped using `_N` suffixes in the filename. For example, a test
with files (`A.scala`, `B_1.scala`, `C_1.java`, `Test_2.scala`) does:
```
scalac         A.scala            -d out
scalac -cp out B_1.scala C_1.java -d out
javac  -cp out C_1.java           -d out
scalac -cp out Test_2.scala       -d out
scala  -cp out Test
```

**Flags**
  - To specify compiler flags such as `-Werror -Xlint`, you can add a comment at the top of your source file of the form:
    `//> using options -Werror -Xlint`
  - Similarly, a `// javac: <flags>` comment in a Java source file passes flags to the Java compiler.
  - A `// filter: <regex>` comment eliminates output lines that match the filter before comparing to the `.check` file.
  - A `// java: <flags>` comment makes a `run` test execute in a separate JVM and passes the additional flags to the `java` command.
  - A `// javaVersion <N[+| - M]>` comment makes partest skip the test if the java version is outside the requested range (e.g. `8`, `15+`, `9 - 11`)

**Common Usage**

To test that no warnings are emitted while compiling a `pos` test, use `-Werror`.
That will fail a `pos` test if there are warnings. Note that `pos` tests do not have `.check` files.

To test that warnings are correctly emitted, use `-Werror` with a `neg` test and `.check` file.
The usual way to create a `.check` file is `partest --update-check`.

To run all tests in `neg` categories from sbt shell:

```
root> partest --neg
```

This might take a couple of minutes to complete. But in a few minutes, you could test 1000+ negative examples,
so it's totally worth your time, especially if you are working on changing error messages.
If you have made a bunch of tests fail by tweaking a message, you can update them in bulk
with `partest --update-check --failed`.

Suppose you're interested in ranges. Here's how you can grep the partests and run them:

```
root> partest --grep range
...
Selected 74 tests drawn from 74 tests matching 'range'
...
# starting 13 tests in pos
ok  3 - pos/lookupswitch.scala
ok  4 - pos/rangepos-patmat.scala
...
```

Another thing you could do is to combine with `--failed` flag to iteratively run
only the failed tests, similar to `testQuick`.

```
root> partest --grep range --failed
```

To inspect the generated files after running the test, add `--debug`:

```
root> partest --debug --verbose test/files/pos/traits.scala
...
# starting 1 test in pos
% scalac pos/traits.scala -d /home/aengelen/dev/scala/test/files/pos/traits-pos.obj
ok 1 - pos/traits.scala
```

See `--help` for more info:

```
root> partest --help
```

Partests are compiled by the bootstrapped `quick` compiler (and `run` partests executed with the `quick` library),
and therefore:

* if you're working on the compiler, you must write a partest, or a `BytecodeTesting` JUnit test which invokes the compiler programmatically; however
* if you're working on the library, a JUnit and/or ScalaCheck is better.

If you're working on Partest itself, note that some of its source files are part of Scala's sbt build, and are compiled when sbt is launched, not via its `compile` command.

#### exploring with REPL

Before or during the test, you might get better insight into the code by starting a REPL session
using the freshly built scala. To start a REPL session from the sbt shell:

```
root> scala
[info] Running scala.tools.nsc.MainGenericRunner -usejavacp
Welcome to Scala 2.13.0-20180304-082722-3debf94 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_151).
Type in expressions for evaluation. Or try :help.

scala> val r = (0.0 to 2.0 by 0.1)
r: scala.collection.immutable.NumericRange[Double] = NumericRange 0.0 to 2.0 by 0.1

scala> r(3)
res0: Double = 0.30000000000000004

scala> for { i <- 1 to 20 } { assert(r.toList(i) == r(i), s"$i failed") }
java.lang.AssertionError: assertion failed: 6 failed
  at scala.Predef$.assert(Predef.scala:248)
  at .$anonfun$res5$1(<console>:1)
  at scala.collection.immutable.Range.foreach$mVc$sp(Range.scala:151)
  ... 33 elided
```

Using this information, you can adjust your test.

### Documentation

This is of course required for new features and enhancements.

Any API additions should include Scaladoc.

Consider updating the package-level doc (in the package object), if appropriate.

### Coding standards

Please follow these standard code standards, though in moderation (scouts quickly learn to let sleeping dogs lie):

Don't violate [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself).
* DRY means: "Don't repeat yourself". 
* Every piece of knowledge must have a single, unambiguous, authoritative representation within a system. 
* Try to only write functionality or algorithms once and reference them (Abstraction) instead of Copy&Paste

Follow the [Boy Scout Rule](https://martinfowler.com/bliki/OpportunisticRefactoring.html).
* "Always leave the code behind in a better state than you found it"
* This translates to using any opportunity possible to improve and clean up the code in front of you

Please also have a look at the [Scala Hacker Guide](https://www.scala-lang.org/contribute/hacker-guide.html) by @xeno-by.

### Clean commits, clean history

A pull request should consist of commits with messages that clearly state what problem the commit resolves and how.

Commit logs should be stated in the active, present tense.

The subject line of a commit message should be no more than 72 characters.
Overall, think of the first line of the commit as a description of the action performed
by the commit on the code base, so use the active voice and the
present tense.  That also makes the commit subjects easy to reuse in
release notes.

For a bugfix, the end of the PR description (that is, the first comment on the PR) should say, "Fixes scala/bug#NNNN", as mentioned above.

NOTE: it's best not to add the issue reference to your commit message, as github will pollute the conversation on the ticket with notifications every time you commit.

If a commit purely refactors and is not intended to change behavior,
say so.

Backports should be tagged as "[backport]".

When working on maintenance branches (e.g., 2.12.x), include "[nomerge]"
if this commit should not be merged forward into the next release
branch.

Here is standard advice on good commit messages:
https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

### Pass Scabot

Our pull request bot, Scabot, automatically builds all of the PR's commits individually on Jenkins. Every commit is expected to pass CI, so we can `git bisect` later.

Click on the little x next to a commit sha to go to the overview of the PR validation job. To diagnose a failure, consult the console output of the job that failed.

See the [scala-jenkins-infra repo](https://github.com/scala/scala-jenkins-infra) and [Scabot repo](https://github.com/scala/scabot) for full details on PR validation.  One tip you should know is that commenting `/rebuild` on a PR asks validation to be run again on the same commits. This is only necessary when a spurious failure occurred.

### Pass code review

Your PR will need to be assigned to one or more reviewers. You can suggest reviewers
yourself; if you're not sure, see the list in [README.md](README.md) or ask on \#scala-contributors (on [Discord](https://discord.com/invite/scala)) or contributors.scala-lang.org (Discourse).

To assign a reviewer, add a "review by @reviewer" to the PR description or in a
comment on your PR.

NOTE: it's best not to @mention in commit messages, as github pings you every time a commit with your @name on it shuffles through the system (even in other repos, on merges, ...).

A reviewer gives the green light using GitHub's [reviews feature](https://help.github.com/articles/about-pull-request-reviews/).

When including review feedback, we typically amend the changes into the existing commit(s)
and `push -f` to the branch. This is to keep the git history clean. Additional commits
are OK if they stand on their own.

Once all these conditions are met, we will merge your changes -- if we
agree with it!  We are available on \#scala-contributors (on [Discord](https://discord.com/invite/scala))
or contributors.scala-lang.org (Discourse) to discuss changes beforehand,
before you put in the coding work.


We use the following labels:

Label                    | Description
-------------------------|:-----------
`welcome`                | added by reviewer / queue curator to welcome someone's first PR (for highlighting in the release notes)
`release-notes`          | added by reviewer / queue curator to make sure this PR is highlighted in the release notes
`on-hold`                | added when this PR should not yet be merged, even though CI is green
`WIP`                    | added by the author if a PR is submitted for CI testing, needs more work to be complete
`assistance-appreciated` | added by the author if help by the community is appreciated to move a change forward
