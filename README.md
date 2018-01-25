# Welcome!
This is the official repository for the [Scala Programming Language](http://www.scala-lang.org).

# How to contribute

To contribute to the Scala standard library, Scala compiler, and Scala Language Specification, please send us a [pull request](https://help.github.com/articles/using-pull-requests/#fork--pull) from your fork of this repository.

We do have to ask you to sign the [Scala CLA](http://www.lightbend.com/contribute/cla/scala) before we can merge any of your work into our code base, to protect its open source nature.

For more information on building and developing the core of Scala, make sure to read
the rest of this README!

In order to get in touch with other Scala contributors, join
[scala/contributors](https://gitter.im/scala/contributors) (Gitter) or post on
[contributors.scala-lang.org](http://contributors.scala-lang.org) (Discourse).

# Reporting issues

Please report bugs at the [scala/bug issue tracker](https://github.com/scala/bug/issues). We use the [scala/scala-dev tracker](https://github.com/scala/scala-dev/issues) for coordinating bigger work items.

# Get in touch!

If you need some help with your PR at any time, please feel free to @-mention anyone from the list below, and we will do our best to help you out:

|                                                                                                 | username                                                       | talk to me about...                               |
--------------------------------------------------------------------------------------------------|----------------------------------------------------------------|---------------------------------------------------|
 <img src="https://avatars.githubusercontent.com/adriaanm"      height="50px" title="Adriaan Moors"/>        | [`@adriaanm`](https://github.com/adriaanm)           | type checker, pattern matcher, infrastructure, language spec |
 <img src="https://avatars.githubusercontent.com/SethTisue"     height="50px" title="Seth Tisue"/>           | [`@SethTisue`](https://github.com/SethTisue)         | build, developer docs, community build, Jenkins, library, the welcome-to-Scala experience |
 <img src="https://avatars.githubusercontent.com/retronym"      height="50px" title="Jason Zaugg"/>          | [`@retronym`](https://github.com/retronym)           | compiler performance, weird compiler bugs, Java 8 lambdas, REPL |
 <img src="https://avatars.githubusercontent.com/Ichoran"       height="50px" title="Rex Kerr"/>             | [`@Ichoran`](https://github.com/Ichoran)             | collections library, performance              |
 <img src="https://avatars.githubusercontent.com/lrytz"         height="50px" title="Lukas Rytz"/>           | [`@lrytz`](https://github.com/lrytz)                 | optimizer, named & default arguments              |
 <img src="https://avatars.githubusercontent.com/VladUreche"    height="50px" title="Vlad Ureche"/>          | [`@VladUreche`](https://github.com/VladUreche)       | specialization, Scaladoc tool |
 <img src="https://avatars.githubusercontent.com/densh"         height="50px" title="Denys Shabalin"/>       | [`@densh`](https://github.com/densh)                 | quasiquotes, parser, string interpolators, macros in standard library |
 <img src="https://avatars.githubusercontent.com/xeno-by"       height="50px" title="Eugene Burmako"/>       | [`@xeno-by`](https://github.com/xeno-by)             | macros and reflection |
 <img src="https://avatars.githubusercontent.com/heathermiller" height="50px" title="Heather Miller"/>      | [`@heathermiller`](https://github.com/heathermiller)  | documentation |
 <img src="https://avatars.githubusercontent.com/dragos"        height="50px" title="Iulian Dragos"/>        | [`@dragos`](https://github.com/dragos)               | specialization, back end |
 <img src="https://avatars.githubusercontent.com/axel22"        height="50px" title="Aleksandr Prokopec"/>   | [`@axel22`](https://github.com/axel22)               | collections, concurrency, specialization |
 <img src="https://avatars.githubusercontent.com/janekdb"       height="50px" title="Janek Bogucki"/>        | [`@janekdb`](https://github.com/janekdb)             | documentation |

P.S.: If you have some spare time to help out around here, we would be delighted to add your name to this list!


# Repository structure

```
scala/
+--build.sbt                 The main sbt build script
+--lib/                      Pre-compiled libraries for the build
+--src/                      All sources
   +---/library              Scala Standard Library
   +---/reflect              Scala Reflection
   +---/compiler             Scala Compiler
   +---/eclipse              Eclipse project files
   +---/intellij             IntelliJ project templates
+--spec/                     The Scala language specification
+--scripts/                  Scripts for the CI jobs (including building releases)
+--test/                     The Scala test suite
   +---/files                Partest tests
   +---/junit                JUnit tests
+--build/                    [Generated] Build output directory
```

# Get ready to contribute

## Requirements

You need the following tools:
  - Java SDK. The baseline version is 8 for both 2.12.x and 2.13.x. It may be possible to use a
    later SDK for local development, but the CI will verify against the baseline
    version.
  - sbt. We recommend the [sbt-extras](https://github.com/paulp/sbt-extras) runner
    script. It provides sensible default jvm options (stack and heap size).

Mac OS X and Linux work. Windows may work if you use Cygwin. Community help with keeping
the build working on Windows is appreciated.

## Tools we use

We are grateful for the following OSS licenses:
  - [JProfiler Java profiler](https://www.ej-technologies.com/products/jprofiler/overview.html)
  - [YourKit Java Profiler](https://www.yourkit.com/java/profiler/)
  - [IntelliJ IDEA](https://www.jetbrains.com/idea/download/)

## Build setup

### Basics

During ordinary development, a new Scala build is built by the
previously released version.  For short we call the previous release
"starr": the stable reference Scala release.  Building with starr is
sufficient for most kinds of changes.

However, a full build of Scala (a *bootstrap*, as performed by our CI)
requires two layers. This guarantees that every Scala version can
build itself. If you change the code generation part of the Scala
compiler, your changes will only show up in the bytecode of the
library and compiler after a bootstrap. See below for how to do a
bootstrap build locally.

For history on how the current scheme was arrived at, see
https://groups.google.com/d/topic/scala-internals/gp5JsM1E0Fo/discussion.

### Using the sbt build

Once you've started an `sbt` session you can run one of the core commands:

  - `compile` compiles all sub-projects (library, reflect, compiler, scaladoc, etc)
  - `scala` / `scalac` run the REPL / compiler directly from sbt (accept options /
    arguments)
  - `dist/mkBin` generates runner scripts (`scala`, `scalac`, etc) in `build/quick/bin`
  - `dist/mkPack` creates a build in the Scala distribution format in `build/pack`
  - `test` runs the JUnit test, `testOnly *immutable.ListTest` runs a subset
  - `partest` runs partest tests (accepts options, try `partest --help`)
  - `publishLocal` publishes a distribution locally (can be used as `scalaVersion` in
    other sbt projects)
    - Optionally `set baseVersionSuffix := "-bin-abcd123-SNAPSHOT"`
      where `abcd123` is the git hash of the revision being published. You can also
      use something custom like `"-bin-mypatch"`. This changes the version number from
      `2.12.2-SNAPSHOT` to something more stable (`2.12.2-bin-abcd123-SNAPSHOT`).
    - Note that the `-bin` string marks the version binary compatible. Using it in
      sbt will cause the `scalaBinaryVersion` to be `2.12`. If the version is not
      binary compatible, we recommend using `-pre`, e.g., `2.13.0-pre-abcd123-SNAPSHOT`.
    - Optionally `set publishArtifact in (Compile, packageDoc) in ThisBuild := false`
      to skip generating / publishing API docs (speeds up the process).

If a command results in an error message like `a module is not authorized to depend on
itself`, it may be that a global SBT plugin (such as [Ensime](http://ensime.org/)) is
resulting in a cyclical dependency. Try disabling global SBT plugins (perhaps by
temporarily commenting them out in `~/.sbt/0.13/plugins/plugins.sbt`).

#### Sandbox

We recommend to keep local test files in the `sandbox` directory which is listed in
the `.gitignore` of the Scala repo.

#### Incremental compilation

Note that sbt's incremental compilation is often too coarse for the Scala compiler
codebase and re-compiles too many files, resulting in long build times (check
[sbt#1104](https://github.com/sbt/sbt/issues/1104) for progress on that front). In the
meantime you can:
  - Enable "Ant mode" in which sbt only re-compiles source files that were modified.
    Create a file `local.sbt` containing the line `antStyle := true`.
  - Use IntelliJ IDEA for incremental compiles (see [IDE Setup](#ide-setup) below) - its
    incremental compiler is a bit less conservative, but usually correct.

#### Bootstrapping locally

To perform a bootstrap using sbt
  - first a build is published either locally or on a temporary repository,
  - then a separate invocation of sbt (using the previously built version as `starr`)
    is used to build / publish the actual build.

Assume the current `starr` version is `2.12.0` (defined in
[versions.properties](versions.properties)) and the current version is `2.12.0-SNAPSHOT`
(defined in [build.sbt](build.sbt)). To perform a local bootstrap:
  - Run `publishLocal` (you may want to specify a custom version suffix and skip
    generating API docs, see above).
  - Quit sbt and start a new sbt instance using `sbt -Dstarr.version=<version>` where
    `<version>` is the version number you published locally.
  - If the version number you published is not binary compatible with the current
    `starr`, `set every scalaBinaryVersion := "2.12.0-M4"`. This is not required if
    the version you published locally is binary compatible, i.e., if the current
    `starr` is a 2.12.x release and not a milestone / RC.

The last step is required to resolve modules (scala-xml, scala-partest, etc). It
assumes that the module releases for the current `starr` work (in terms of binary
compatibility) with the local starr that you published locally. A full bootstrap
requires re-building the all the modules. On our CI this is handled by the
[bootstrap](scripts/jobs/integrate/bootstrap) script, but it (currently) cannot
be easily executed locally.

### IDE setup

You may use IntelliJ IDEA (see [src/intellij/README.md](src/intellij/README.md)),
the Scala IDE for Eclipse (see [src/eclipse/README.md](src/eclipse/README.md)),
or ENSIME (see [this page on the ENSIME site](http://ensime.org/editors/)).

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
  - The ["Scala Hacker Guide"](http://scala-lang.org/contribute/hacker-guide.html)
    covers some of the same ground as this README, but in greater detail and in a more
    tutorial style, using a running example.
  - [Scala documentation site](http://docs.scala-lang.org)

# Scala CI

Once you submit a PR your commits will be automatically tested by the Scala CI.

If you see a spurious build failure, you can post `/rebuild` as a PR comment.
The [scabot README](https://github.com/scala/scabot) lists all available commands.

If you'd like to test your patch before having everything polished for review,
feel free to submit a PR and add the `WIP` label. In case your WIP branch contains
a large number of commits (that you didn't clean up / squash yet for review),
consider adding `[ci: last-only]` to the PR title. That way only the last commit
will be tested, saving some energy and CI-resources. Note that inactive WIP PRs
will be closed eventually, which does not mean the change is being rejected.

CI performs a full bootstrap. The first task, `validate-publish-core`, publishes
a build of your commit to the temporary repository
https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots.
Note that this build is not yet bootstrapped, its bytecode is built using the
current `starr`. The version number is `2.12.2-bin-abcd123-SNAPSHOT` where `abcd123`
is the commit hash. For binary incompatible builds, the version number is
`2.13.0-pre-abcd123-SNAPSHOT`.

You can use Scala builds in the validation repository locally by adding a resolver
and specifying the corresponding `scalaVersion`:

```
$ sbt
> set resolvers += "pr" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/"
> set scalaVersion := "2.12.2-bin-abcd123-SNAPSHOT"
> console
```

Note that the scala modules are currently not built/published against the
tested version during CI validation.

## Nightly builds

The Scala CI builds nightly download releases (including all modules) and publishes
them to the following locations:
  - [2.12.x](http://www.scala-lang.org/files/archive/nightly/2.12.x/?C=M;O=D)
  - [2.13.x](http://www.scala-lang.org/files/archive/nightly/2.13.x/?C=M;O=D)

The CI also publishes nightly API docs:
  - [2.12.x](http://www.scala-lang.org/files/archive/nightly/2.12.x/api/?C=M;O=D)
    - [symlink to the latest](http://www.scala-lang.org/files/archive/nightly/2.12.x/api/2.12.x/)
  - [2.13.x](http://www.scala-lang.org/files/archive/nightly/2.13.x/api/?C=M;O=D)
    - [symlink to the latest](http://www.scala-lang.org/files/archive/nightly/2.13.x/api/2.13.x/)

Using a nightly build in sbt is explained in
[this Stack Overflow answer](http://stackoverflow.com/questions/40622878)

## Scala CI internals

The Scala CI runs as a Jenkins instance on [scala-ci.typesafe.com](https://scala-ci.typesafe.com/),
configured by a chef cookbook at [scala/scala-jenkins-infra](https://github.com/scala/scala-jenkins-infra).

The build bot that watches PRs, triggers testing builds and applies the "reviewed" label
after an LGTM comment is in the [scala/scabot](https://github.com/scala/scabot) repo.

## Community build

The Scala community build is a central element for testing Scala
releases. A community build can be launched for any Scala commit, even
before the commit's PR has been merged. That commit is then used to
build a large number of open-source projects from source and run their
test suites.

To request a community build run on your PR, just ask in a comment on
the PR and a Scala team member will take care of
it. ([details](https://github.com/scala/community-builds/wiki#can-i-run-it-against-a-pull-request-in-scalascala))

Community builds run on the Scala Jenkins instance.  The jobs are
named `..-integrate-community-build`. See the
[scala/community-builds](https://github.com/scala/community-builds)
repo.
