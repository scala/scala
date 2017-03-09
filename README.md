# Welcome!
This is the official repository for the [Scala Programming Language](http://www.scala-lang.org).

# How to contribute

To contribute to the Scala Standard Library, Scala Compiler and Scala Language Specification, please send us a [pull request](https://help.github.com/articles/using-pull-requests/#fork--pull) from your fork of this repository! We do have to ask you to sign the [Scala CLA](http://www.lightbend.com/contribute/cla/scala) before we can merge any of your work into our code base, to protect its open source nature.

For more information on building and developing the core of Scala, make sure to read
the rest of this README!

In order to get in touch with Scala contributors, join the
[scala/contributors](https://gitter.im/scala/contributors) gitter channel or post on the
[scala-internals mailing list](http://www.scala-lang.org/community/).

# Reporting issues

We're still using Jira for issue reporting, so please [report any issues](https://issues.scala-lang.org) over there.
(We would love to start using GitHub Issues, but we're too resource-constrained to take on this migration right now.)

# Get in touch!
If you need some help with your PR at any time, please feel free to @-mention anyone from the list below, and we will do our best to help you out:

                                                                                                  | username                                                       | talk to me about...                               |
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
 <img src="https://avatars.githubusercontent.com/dickwall"      height="50px" title="Dick Wall"/>            | [`@dickwall`](https://github.com/dickwall)           | process & community, documentation |
 <img src="https://avatars.githubusercontent.com/dragos"        height="50px" title="Iulian Dragos"/>        | [`@dragos`](https://github.com/dragos)               | specialization, back end |
 <img src="https://avatars.githubusercontent.com/axel22"        height="50px" title="Aleksandr Prokopec"/>   | [`@axel22`](https://github.com/axel22)               | collections, concurrency, specialization |
 <img src="https://avatars.githubusercontent.com/janekdb"       height="50px" title="Janek Bogucki"/>        | [`@janekdb`](https://github.com/janekdb)             | documentation |

P.S.: If you have some spare time to help out around here, we would be delighted to add your name to this list!

# Repository structure

```
scala/
+--build.sbt                 The main sbt build script
+--build.xml                 The deprecated Ant build script
+--pull-binary-libs.sh       Pulls binary artifacts from remote repository, used by build scripts
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

# Get Ready to Contribute

## Requirements

You need the following tools:
  - A Java SDK. The baseline version is 6 for 2.11.x, 8 for 2.12.x. It's possible
  to use a later SDK for local development, but the CI will verify against the baseline
  version.
  - sbt, we recommend the [sbt-extras](https://github.com/paulp/sbt-extras) runner
  script. It provides sensible default jvm options (stack and heap size).
  - curl (for `./pull-binary-libs.sh`, used by the sbt / ant build).
  - Apache Ant (version 1.9.x, minimum 1.9.3; Ant 1.10+ doesn't work on Java 6) if you are using the ant build.

Mac OS X and Linux work. Windows may work if you use Cygwin. Community help with keeping
the build working on Windows is appreciated.

## Build Setup

### Basics

Scala is built in layers, where each layer is a complete Scala compiler and library.
Here is a short description of the layers, from bottom to top:

  - `starr`: the stable reference Scala release. We use an official release of
    Scala (specified by `starr.version` in [versions.properties](versions.properties)),
    downloaded from the Central Repository.
  - `locker` (deprecated, only in ant): an intermediate layer that existed in the
    ant build to perform a bootstrap.
  - `quick`: the development layer which is incrementally built when working on
    changes in the compiler or library.
  - `strap` (deprecated, only in ant) : a test layer used to check stability of
    the build.

The sbt build uses `starr` to build `quick`. This is sufficient for most development
scenarios: changes to the library or the compiler can be tested by running the `quick`
Scala (see below for how to do that).

However, a full build of Scala (a *bootstrap*, as performed by our CI) requires two
layers. This guarantees that every Scala version can build itself. If you change the
code generation part of the Scala compiler, your changes will only reflect in the
bytecode of the library and compiler after a bootstrap. See below for how to create
a bootstrap build locally.

### Using the Sbt Build

Core commands:
  - `compile` compiles all sub-projects (library, reflect, compiler, scaladoc, etc)
  - `scala` / `scalac` run the REPL / compiler directly from sbt (accept options /
    arguments)
  - `dist/mkBin` generates runner scripts (`scala`, `scalac`, etc) in `build/quick/bin`
  - `dist/mkPack` creates a build in the Scala distribution format in `build/pack`
  - `test` runs the JUnit test, `testOnly *immutable.ListTest` runs a subset
  - `partest` runs partest tests (accepts options, try `partest --help`)
  - `publishLocal` publishes a distribution locally (can be used as `scalaVersion` in
    other sbt projects)
    - Optionally `set VersionUtil.baseVersionSuffix in Global := "abcd123-SNAPSHOT"`
      where `abcd123` is the git hash of the revision being published. You can also
      use something custom like `"mypatch"`. This changes the version number from
      `2.12.0-SNAPSHOT` to something more stable (`2.12.0-abcd123-SNAPSHOT`).
    - Optionally `set publishArtifact in (Compile, packageDoc) in ThisBuild := false`
      to skip generating / publishing API docs (speeds up the process).

#### Sandbox

We recommend to keep local test files in the `sandbox` directory which is listed in
the `.gitignore` of the Scala repo.

#### Incremental Compilation

Note that sbt's incremental compilation is often too coarse for the Scala compiler
codebase and re-compiles too many files, resulting in long build times (check
[sbt#1104](https://github.com/sbt/sbt/issues/1104) for progress on that front). In the
meantime you can:
  - Enable "ant mode" in which sbt only re-compiles source files that were modified.
    Create a file `local.sbt` containing the line `antStyle := true`.
    Add an entry `local.sbt` to your `~/.gitignore`.
  - Use IntelliJ IDEA for incremental compiles (see [IDE Setup](#ide-setup) below) - its
    incremental compiler is a bit less conservative, but usually correct.

#### Local Bootstrap Build

To perform a bootstrap using sbt
  - first a build is published either locally or on a temporary repository,
  - then a separate invocation of sbt (using the previously built version as `starr`)
    is used to build / publish the actual build.

Assume the current `starr` version is `2.12.0-M4` (defined in
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

### IDE Setup

You may use IntelliJ IDEA ([src/intellij/README.md](src/intellij/README.md)) or the
Scala IDE for Eclipse (see [src/eclipse/README.md](src/eclipse/README.md)).

In order to use IntelliJ's incremental compiler:
  - run `dist/mkBin` in sbt to get a build and the runner scripts in `build/quick/bin`
  - run "Build" - "Make Project" in IntelliJ

Now you can edit and build in IntelliJ and use the scripts (compiler, REPL) to
directly test your changes. You can also run the `scala`, `scalac` and `partest`
commands in sbt. Enable "ant mode" (explained above) to prevent sbt's incremental
compiler from re-compiling (too many) files before each `partest` invocation.

# Coding Guidelines

Our guidelines for contributing are explained in [CONTRIBUTING.md](CONTRIBUTING.md).
It contains useful information on our coding standards, testing, documentation, how
we use git and GitHub and how to get your code reviewed.

You may also want to check out the following resources:
  - The ["Scala Hacker Guide"](http://scala-lang.org/contribute/hacker-guide.html)
    covers some of the same ground as this README, but in greater detail and in a more
    tutorial style, using a running example.
  - [Scala documentation site](http://docs.scala-lang.org)

# Scala CI

Once you submit a PR your commits will are automatically tested by the Scala CI.

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
current `starr`. The version number is `2.12.0-abcd123-SNAPSHOT` where `abcd123`
is the commit hash.

You can use Scala builds in the validation repository locally by adding a resolver
and specifying the corresponding `scalaVersion`:

```
$ sbt
> set resolvers += "pr" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/"
> set scalaVersion := "2.12.0-abcd123-SNAPSHOT"
> console
```

Note that the scala modules are currently not built / published against the
tested version during CI validation.

## Nightly Builds

The Scala CI builds nightly download releases (including all modules) and publishes
them to the following locations:
  - [2.12.x](http://www.scala-lang.org/files/archive/nightly/2.12.x/?C=M;O=D)
  - [2.11.x](http://www.scala-lang.org/files/archive/nightly/2.11.x/?C=M;O=D)

The CI also publishes nightly API docs:
  - [2.12.x](http://www.scala-lang.org/files/archive/nightly/2.12.x/api/?C=M;O=D)
    - [symlink to the latest](http://www.scala-lang.org/files/archive/nightly/2.12.x/api/2.12.x/)
  - [2.11.x](http://www.scala-lang.org/files/archive/nightly/2.11.x/api/?C=M;O=D)
    - [symlink to the latest](http://www.scala-lang.org/files/archive/nightly/2.11.x/api/2.11.x/)

Note that we currently don't publish nightly (or SNAPSHOT) builds in maven or ivy
format to any repository. You can track progress on this front at
[scala-jenkins-infra#133](https://github.com/scala/scala-jenkins-infra/issues/133)
and [scala-dev#68](https://github.com/scala/scala-dev/issues/68).

## Scala CI Internals

The Scala CI runs as a Jenkins instance on [scala-ci.typesafe.com](https://scala-ci.typesafe.com/),
configured by a chef cookbook at [scala/scala-jenkins-infra](https://github.com/scala/scala-jenkins-infra).

The build bot that watches PRs, triggers testing builds and applies the "reviewed" label
after an LGTM comment is in the [scala/scabot](https://github.com/scala/scabot) repo.

## Community Build

The community build is a central element for testing Scala releases. A community
build can be launched for any Scala revision / commit. It first builds the Scala
library and compiler and then uses that Scala version to build a large number of
open-source projects from source.

Community builds run on the Scala Jenkins instance, the jobs are named
`..-integrate-community-build`. The community build definitions specifying which
projects are built are in the
[scala/community-builds](https://github.com/scala/community-builds) repo.
