# This is Scala 2! Welcome!

This is the home of the [Scala 2](https://www.scala-lang.org)
standard library, compiler, and language spec.

For Scala 3, visit [scala/scala3](https://github.com/scala/scala3).

# How to contribute

Issues and bug reports for Scala 2 are located in [scala/bug](https://github.com/scala/bug). That tracker is also where new contributors may find issues to work on: [good first issues](https://github.com/scala/bug/labels/good%20first%20issue), [help wanted](https://github.com/scala/bug/labels/help%20wanted).

For coordinating broader efforts, we also use the [scala/scala-dev tracker](https://github.com/scala/scala-dev/issues).

To contribute here, please open a [pull request](https://help.github.com/articles/using-pull-requests/#fork--pull) from your fork of this repository.

Be aware that we can't accept additions to the standard library, only modifications to existing code.  Binary compatibility forbids adding new public classes or public methods. Additions are made to [scala-library-next](https://github.com/scala/scala-library-next) instead.

We require that you sign the [Scala CLA](https://www.lightbend.com/contribute/cla/scala) before we can merge any of your work, to protect Scala's future as open source software.

The general workflow is as follows.
1. Find/file an issue in scala/bug (or submit a well-documented PR right away!).
2. Fork the scala/scala repo.
3. Push your changes to a branch in your forked repo. For coding guidelines, go [here](https://github.com/scala/scala#coding-guidelines).
4. Submit a pull request to scala/scala from your forked repo.

For more information on building and developing the core of Scala, read the rest of this README, especially for [setting up your machine](https://github.com/scala/scala#get-ready-to-contribute)!


# Get in touch!

In order to get in touch with other Scala contributors, join the
\#scala-contributors channel on the [Scala Discord](https://discord.com/invite/scala) chat, or post on
[contributors.scala-lang.org](https://contributors.scala-lang.org) (Discourse).

If you need some help with your PR at any time, please feel free to @-mention anyone from the list below, and we will do our best to help you out:

|                                                                                                 | username                                                       | talk to me about...                               |
--------------------------------------------------------------------------------------------------|----------------------------------------------------------------|---------------------------------------------------|
 <img src="https://avatars.githubusercontent.com/lrytz"         height="50px" title="Lukas Rytz"/>           | [`@lrytz`](https://github.com/lrytz)                 | back end, optimizer, named & default arguments, reporters       |
 <img src="https://avatars.githubusercontent.com/retronym"      height="50px" title="Jason Zaugg"/>          | [`@retronym`](https://github.com/retronym)           | 2.12.x branch, compiler performance, weird compiler bugs, lambdas |
 <img src="https://avatars.githubusercontent.com/SethTisue"     height="50px" title="Seth Tisue"/>           | [`@SethTisue`](https://github.com/SethTisue)         | getting started, build, CI, community build, Jenkins, docs, library, REPL |
 <img src="https://avatars.githubusercontent.com/dwijnand"      height="50px" title="Dale Wijnand"/>         | [`@dwijnand`](https://github.com/dwijnand)           | pattern matcher, MiMa, partest |
 <img src="https://avatars.githubusercontent.com/som-snytt"     height="50px" title="Som Snytt"/>            | [`@som-snytt`](https://github.com/som-snytt)         | warnings/lints/errors, REPL, compiler options, compiler internals, partest |
 <img src="https://avatars.githubusercontent.com/Ichoran"       height="50px" title="Rex Kerr"/>             | [`@Ichoran`](https://github.com/Ichoran)             | collections library, performance              |
 <img src="https://avatars.githubusercontent.com/viktorklang"   height="50px" title="Viktor Klang"/>         | [`@viktorklang`](https://github.com/viktorklang)     | concurrency, futures |
 <img src="https://avatars.githubusercontent.com/sjrd"          height="50px" title="Sébastien Doeraene"/>   | [`@sjrd`](https://github.com/sjrd)                   | interactions with Scala.js |
 <img src="https://avatars.githubusercontent.com/NthPortal"     height="50px" title="Princess \| April"/>    | [`@NthPortal`](https://github.com/NthPortal)         | library, concurrency, `scala.math`, `LazyList`, `Using`, warnings |
 <img src="https://avatars.githubusercontent.com/bishabosha"    height="50px" title="Jamie Thompson"/>       | [`@bishabosha`](https://github.com/bishabosha)       | TASTy reader |
 <img src="https://avatars.githubusercontent.com/joroKr21"      height="50px" title="Georgi Krastev"/>       | [`@joroKr21`](https://github.com/joroKr21)           | higher-kinded types, implicits, variance |

P.S.: If you have some spare time to help out around here, we would be delighted to add your name to this list!


# Branches

Target the oldest branch you would like your changes to end up in. We periodically merge forward from older release branches (e.g., 2.12.x) to new ones (e.g. 2.13.x).

If your change is difficult to merge forward, you may be asked to also submit a separate PR targeting the newer branch.

If your change is version-specific and shouldn't be merged forward, put `[nomerge]` in the PR name.

If your change is a backport from a newer branch and thus doesn't need to be merged forward, put `[backport]` in the PR name.

## Choosing a branch

Most changes should target 2.13.x. We are increasingly reluctant to target 2.12.x unless there is a special reason (e.g. if an especially bad bug is found, or if there is commercial sponsorship).

The 2.11.x branch is now [inactive](https://github.com/scala/scala-dev/issues/451) and no further 2.11.x releases are planned (unless unusual, unforeseeable circumstances arise). You should not target 2.11.x without asking maintainers first.


# Repository structure

Most importantly:

```
scala/
+--build.sbt                 The main sbt build definition
+--project/                  The rest of the sbt build
+--src/                      All sources
   +---/library              Scala Standard Library
   +---/reflect              Scala Reflection
   +---/compiler             Scala Compiler
+--test/                     The Scala test suite
   +---/files                Partest tests
   +---/junit                JUnit tests
   +---/scalacheck           ScalaCheck tests
+--spec/                     The Scala language specification
```

but also:

```
scala/
   +---/library-aux          Scala Auxiliary Library, for bootstrapping and documentation purposes
   +---/interactive          Scala Interactive Compiler, for clients such as an IDE (aka Presentation Compiler)
   +---/intellij             IntelliJ project templates
   +---/manual               Scala's runner scripts "man" (manual) pages
   +---/partest              Scala's internal parallel testing framework
   +---/partest-javaagent    Partest's helper java agent
   +---/repl                 Scala REPL core
   +---/repl-frontend        Scala REPL frontend
   +---/scaladoc             Scala's documentation tool
   +---/scalap               Scala's class file decompiler
   +---/testkit              Scala's unit-testing kit
+--admin/                    Scripts for the CI jobs and releasing
+--doc/                      Additional licenses and copyrights
+--scripts/                  Scripts for the CI jobs and releasing
+--tools/                    Scripts useful for local development
+--build/                    Build products
+--dist/                     Build products
+--target/                   Build products
```

# Get ready to contribute

## Requirements

You need the following tools:
  - Java SDK. The baseline version is 8 for both 2.12.x and 2.13.x. It is almost always fine
    to use a later SDK such as 11 or 15 for local development. CI will verify against the
    baseline version.
  - sbt

MacOS and Linux work. Windows may work if you use Cygwin. Community help with keeping
the build working on Windows and documenting any needed setup is appreciated.

## Tools we use

We are grateful for the following OSS licenses:
  - [JProfiler Java profiler](https://www.ej-technologies.com/products/jprofiler/overview.html)
  - [YourKit Java Profiler](https://www.yourkit.com/java/profiler/)
  - [IntelliJ IDEA](https://www.jetbrains.com/idea/download/)
  - [![Revved up by Develocity](https://img.shields.io/badge/Revved%20up%20by-Develocity-06A0CE?logo=Gradle&labelColor=02303A)](https://develocity.scala-lang.org)

## Build setup

### Basics

During ordinary development, a new Scala build is built by the
previously released version, known as the "reference compiler" or,
slangily, as "STARR" (stable reference release).  Building with STARR
is sufficient for most kinds of changes.

However, a full build of Scala is _bootstrapped_.  Bootstrapping has
two steps: first, build with STARR; then, build again using the
freshly built compiler, leaving STARR behind.  This guarantees that
every Scala version can build itself.

If you change the code generation part of the Scala compiler, your
changes will only show up in the bytecode of the library and compiler
after a bootstrap. Our CI does a bootstrapped build.

**Bootstrapping locally**: To perform a bootstrap, run `restarrFull`
within an sbt session.  This will build and publish the Scala
distribution to your local artifact repository and then switch sbt to
use that version as its new `scalaVersion`.  You may then revert back
with `reload`.  Note `restarrFull` will also write the STARR version
to `buildcharacter.properties` so you can switch back to it with
`restarr` without republishing.  This will switch the sbt session to
use the `build-restarr` and `target-restarr` directories instead of
`build` and `target`, which avoids wiping out classfiles and
incremental metadata.  IntelliJ will continue to be configured to
compile and run tests using the starr version in
`versions.properties`.

For history on how the current scheme was arrived at, see
https://groups.google.com/d/topic/scala-internals/gp5JsM1E0Fo/discussion.

**Building with fatal warnings**: To make warnings in the project fatal (i.e. turn them into errors), run `set Global / fatalWarnings := true` in sbt (replace `Global` with the name of a module—such as `reflect`—to only make warnings fatal for that module). To disable fatal warnings again, either `reload` sbt, or run `set Global / fatalWarnings := false` (again, replace `Global` with the name of a module if you only enabled fatal warnings for that module). CI always has fatal warnings enabled.

### Using the sbt build

Once you've started an `sbt` session you can run one of the core commands:

  - `compile` compiles all sub-projects (library, reflect, compiler, scaladoc, etc)
  - `scala` / `scalac` run the REPL / compiler directly from sbt (accept options /
    arguments)
  - `enableOptimizer` reloads the build with the Scala optimizer enabled. Our releases are built this way. Enable this when working on compiler performance improvements. When the optimizer is enabled the build will be slower and incremental builds can be incorrect.
  - `setupPublishCore` runs `enableOptimizer` and configures a version number based on the current Git SHA. Often used as part of bootstrapping: `sbt setupPublishCore publishLocal && sbt -Dstarr.version=<VERSION> testAll`
  - `dist/mkBin` generates runner scripts (`scala`, `scalac`, etc) in `build/quick/bin`
  - `dist/mkPack` creates a build in the Scala distribution format in `build/pack`
  - `junit/test` runs the JUnit tests; `junit/testOnly *Foo` runs a subset
  - `scalacheck/test` runs scalacheck tests, use `testOnly` to run a subset
  - `partest` runs partest tests (accepts options, try `partest --help`)
  - `publishLocal` publishes a distribution locally (can be used as `scalaVersion` in
    other sbt projects)
    - Optionally `set baseVersionSuffix := "bin-abcd123-SNAPSHOT"`
      where `abcd123` is the git hash of the revision being published. You can also
      use something custom like `"bin-mypatch"`. This changes the version number from
      `2.13.2-SNAPSHOT` to something more stable (`2.13.2-bin-abcd123-SNAPSHOT`).
    - Note that the `-bin` string marks the version binary compatible. Using it in
      sbt will cause the `scalaBinaryVersion` to be `2.13`. If the version is not
      binary compatible, we recommend using `-pre`, e.g., `2.14.0-pre-abcd123-SNAPSHOT`.
    - Optionally `set ThisBuild / Compile / packageDoc / publishArtifact := false`
      to skip generating / publishing API docs (speeds up the process).

If a command results in an error message like `a module is not authorized to depend on
itself`, it may be that a global sbt plugin is causing
a cyclical dependency. Try disabling global sbt plugins (perhaps by
temporarily commenting them out in `~/.sbt/1.0/plugins/plugins.sbt`).

#### Sandbox

We recommend keeping local test files in the `sandbox` directory which is listed in
the `.gitignore` of the Scala repo.

#### Incremental compilation

Note that sbt's incremental compilation is often too coarse for the Scala compiler
codebase and re-compiles too many files, resulting in long build times (check
[sbt#1104](https://github.com/sbt/sbt/issues/1104) for progress on that front). In the
meantime you can:
  - Use IntelliJ IDEA for incremental compiles (see [IDE Setup](#ide-setup) below) - its
    incremental compiler is a bit less conservative, but usually correct.

### IDE setup

We suggest using IntelliJ IDEA (see
[src/intellij/README.md](src/intellij/README.md)).

[Metals](https://scalameta.org/metals/) may also work, but we don't
yet have instructions or sample configuration for that. A pull request
in this area would be exceedingly welcome. In the meantime, we are
collecting guidance at
[scala/scala-dev#668](https://github.com/scala/scala-dev/issues/668).

In order to use IntelliJ's incremental compiler:
  - run `dist/mkBin` in sbt to get a build and the runner scripts in `build/quick/bin`
  - run "Build" - "Make Project" in IntelliJ

Now you can edit and build in IntelliJ and use the scripts (compiler, REPL) to
directly test your changes. You can also run the `scala`, `scalac` and `partest`
commands in sbt. Enable "Ant mode" (explained above) to prevent sbt's incremental
compiler from re-compiling (too many) files before each `partest` invocation.

# Coding guidelines

Our guidelines for contributing are explained in [CONTRIBUTING.md](CONTRIBUTING.md).
It contains useful information on our coding standards, testing, documentation, how
we use git and GitHub and how to get your code reviewed.

You may also want to check out the following resources:
  - The ["Scala Hacker Guide"](https://scala-lang.org/contribute/hacker-guide.html)
    covers some of the same ground as this README, but in greater detail and in a more
    tutorial style, using a running example.
  - [Scala documentation site](https://docs.scala-lang.org)

# Scala CI

[![Build Status](https://travis-ci.com/scala/scala.svg?branch=2.13.x)](https://travis-ci.com/scala/scala)

Once you submit a PR your commits will be automatically tested by the Scala CI.

Our CI setup is always evolving.  See
[scala/scala-dev#751](https://github.com/scala/scala-dev/issues/751)
for more details on how things currently work and how we expect they
might change.

If you see a spurious failure on Jenkins, you can post `/rebuild` as a PR comment.
The [scabot README](https://github.com/scala/scabot) lists all available commands.

If you'd like to test your patch before having everything polished for review,
you can have Travis CI build your branch (make sure you have a fork and have Travis CI
enabled for branch builds on it first, and then push your branch).  Also
feel free to submit a draft PR. In case your draft branch contains
a large number of commits (that you didn't clean up / squash yet for review),
consider adding `[ci: last-only]` to the PR title. That way only the last commit
will be tested, saving some energy and CI-resources. Note that inactive draft PRs
will be closed eventually, which does not mean the change is being rejected.

CI performs a compiler bootstrap. The first task, `validatePublishCore`, publishes
a build of your commit to the temporary repository
https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots.
Note that this build is not yet bootstrapped, its bytecode is built using the
current STARR. The version number is `2.13.2-bin-abcd123-SNAPSHOT` where `abcd123`
is the commit hash. For binary incompatible builds, the version number is
`2.14.0-pre-abcd123-SNAPSHOT`.

You can use Scala builds in the validation repository locally by adding a resolver
and specifying the corresponding `scalaVersion`:

```
$ sbt
> set resolvers += "pr" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/"
> set scalaVersion := "2.12.2-bin-abcd123-SNAPSHOT"
> console
```

## "Nightly" builds

The Scala CI publishes these to
https://scala-ci.typesafe.com/artifactory/scala-integration/ .

Using a nightly build in sbt and other tools is explained on this
[doc page](https://docs.scala-lang.org/overviews/core/nightlies.html).

Although we casually refer to these as "nightly" builds, they aren't
actually built nightly, but "mergely".  That is to say, a build is
published for every merged PR.

## Scala CI internals

The Scala CI runs as a Jenkins instance on [scala-ci.typesafe.com](https://scala-ci.typesafe.com/),
configured by a chef cookbook at [scala/scala-jenkins-infra](https://github.com/scala/scala-jenkins-infra).

The build bot that watches PRs, triggers testing builds and applies the "reviewed" label
after an LGTM comment is in the [scala/scabot](https://github.com/scala/scabot) repo.

## Community build

The Scala community build is an important method for testing Scala
releases. A community build can be launched for any Scala commit, even
before the commit's PR has been merged. That commit is then used to
build a large number of open-source projects from source and run their
test suites.

To request a community build run on your PR, just ask in a comment on
the PR and a Scala team member (probably @SethTisue) will take care of
it. ([details](https://github.com/scala/community-builds/wiki#can-i-run-it-against-a-pull-request-in-scalascala))

Community builds run on the Scala Jenkins instance.  The jobs are
named `..-integrate-community-build`. See the
[scala/community-builds](https://github.com/scala/community-builds)
repo.
