# Welcome!
This is the official repository for the [Scala Programming Language](http://www.scala-lang.org).

# How to contribute

To contribute to the Scala Standard Library, Scala Compiler and Scala Language Specification, please send us a [pull request](https://help.github.com/articles/using-pull-requests/#fork--pull) from your fork of this repository! We do have to ask you to sign the [Scala CLA](http://www.lightbend.com/contribute/cla/scala) before we can merge any of your work into our code base, to protect its open source nature.

For more information on building and developing the core of Scala, read on!

Please also check out:

* our [guidelines for contributing](CONTRIBUTING.md).
* the ["Scala Hacker Guide"](http://scala-lang.org/contribute/hacker-guide.html) covers some of the same ground as this README, but in greater detail and in a more tutorial style, using a running example.

# Reporting issues

We're still using Jira for issue reporting, so please [report any issues](https://issues.scala-lang.org) over there.
(We would love to start using GitHub Issues, but we're too resource-constrained to take on this migration right now.)

# Get in touch!
If you need some help with your PR at any time, please feel free to @-mention anyone from the list below (or simply `@scala/team-core-scala`), and we will do our best to help you out:

                                                                                                  | username                                                       | talk to me about...                               |
--------------------------------------------------------------------------------------------------|----------------------------------------------------------------|---------------------------------------------------|
 <img src="https://avatars.githubusercontent.com/adriaanm"     height="50px" title="Adriaan Moors"/>        | [`@adriaanm`](https://github.com/adriaanm)           | type checker, pattern matcher, infrastructure, language spec |
 <img src="https://avatars.githubusercontent.com/SethTisue"    height="50px" title="Seth Tisue"/>           | [`@SethTisue`](https://github.com/SethTisue)         | build, developer docs, community build, Jenkins, library, the welcome-to-Scala experience |
 <img src="https://avatars.githubusercontent.com/retronym"     height="50px" title="Jason Zaugg"/>          | [`@retronym`](https://github.com/retronym)           | compiler performance, weird compiler bugs, Java 8 lambdas, REPL |
 <img src="https://avatars.githubusercontent.com/Ichoran"      height="50px" title="Rex Kerr"/>             | [`@Ichoran`](https://github.com/Ichoran)             | collections library, performance              |
 <img src="https://avatars.githubusercontent.com/lrytz"        height="50px" title="Lukas Rytz"/>           | [`@lrytz`](https://github.com/lrytz)                 | optimizer, named & default arguments              |
 <img src="https://avatars.githubusercontent.com/VladUreche"   height="50px" title="Vlad Ureche"/>          | [`@VladUreche`](https://github.com/VladUreche)       | specialization, Scaladoc tool |
 <img src="https://avatars.githubusercontent.com/densh"        height="50px" title="Denys Shabalin"/>       | [`@densh`](https://github.com/densh)                 | quasiquotes, parser, string interpolators, macros in standard library |
 <img src="https://avatars.githubusercontent.com/xeno-by"      height="50px" title="Eugene Burmako"/>       | [`@xeno-by`](https://github.com/xeno-by)             | macros and reflection |
 <img src="https://avatars.githubusercontent.com/heathermiller" height="50px" title="Heather Miller"/>      | [`@heathermiller`](https://github.com/heathermiller) | documentation |
 <img src="https://avatars.githubusercontent.com/dickwall"     height="50px" title="Dick Wall"/>            | [`@dickwall`](https://github.com/dickwall)           | process & community, documentation |
 <img src="https://avatars.githubusercontent.com/dragos"       height="50px" title="Iulian Dragos"/>        | [`@dragos`](https://github.com/dragos)               | specialization, back end |
 <img src="https://avatars.githubusercontent.com/axel22"       height="50px" title="Aleksandr Prokopec"/>   | [`@axel22`](https://github.com/axel22)               | collections, concurrency, specialization |
 <img src="https://avatars.githubusercontent.com/janekdb"      height="50px" title="Janek Bogucki"/>        | [`@janekdb`](https://github.com/janekdb)             | documentation |

P.S.: If you have some spare time to help out around here, we would be delighted to add your name to this list!

# Handy Links
  - [A wealth of documentation](http://docs.scala-lang.org)
  - [mailing lists](http://www.scala-lang.org/community/)
  - [Gitter room for Scala contributors](https://gitter.im/scala/contributors)
  - [Scala CI](https://scala-ci.typesafe.com/)
  - download the latest nightlies:
      - [2.11.x](http://www.scala-lang.org/files/archive/nightly/2.11.x/)
      - [2.12.x](http://www.scala-lang.org/files/archive/nightly/2.12.x/)

# Repository structure

```
scala/
+--build.xml                 The main Ant build script, see also under src/build.
+--pull-binary-libs.sh       Pulls binary artifacts from remote repository.
+--lib/                      Pre-compiled libraries for the build.
+--src/                      All sources.
   +---/library              Scala Standard Library.
   +---/reflect              Scala Reflection.
   +---/compiler             Scala Compiler.
   +---/eclipse              Eclipse project files.
   +---/intellij             IntelliJ project templates.
+--scripts/                  Scripts for the CI jobs (including building releases)
+--test/                     The Scala test suite.
+--build/                    [Generated] Build products output directory for ant.
+--dist/                     [Generated] The destination folder for Scala distributions.
```

# How we roll

## Requirements

You'll need a Java SDK.  The baseline version is 6 for 2.11.x, 8 for
2.12.x. (It's also possible to use a later SDK for local development,
but the CI will verify against the baseline version.)

You'll also need Apache Ant (version 1.9.0 or above) and curl (for `./pull-binary-libs.sh`).

Mac OS X and Linux work. Windows may work if you use Cygwin. (Community help with keeping the build working on Windows is appreciated.)

## Git Hygiene

As git history is forever, we take great pride in the quality of the commits we merge into the repository. The title of your commit will be read hundreds (of thousands? :-)) of times, so it pays off to spend just a little bit more time to polish it, making it descriptive and concise. Please take a minute to read the advice [most projects agree on](https://github.com/erlang/otp/wiki/Writing-good-commit-messages), and stick to 72 or fewer characters for the first line, wrapping subsequent ones at 80 (at most).

When not sure how to formulate your commit message, imagine you're writing a bullet item for the next release notes, or describing what the commit does to the code base (use active verbs in the present tense). When your commit title is featured in the next release notes, it will be read by a lot of curious Scala users, looking for the latest improvements. Satisfy their thirst for information with as few words as possible! Also, a commit should convey clearly to your (future) fellow contributors what it does to the code base.

Writing the commit message is a great sanity check that the commit is of the right size. If it does too many things, the description will be unwieldy and tedious to write. Chop it up (`git add -u --patch` and `git rebase` are your friends) and simplify!

To pinpoint bugs, we often use git bisect, which is only effective when we can count on each commit building (and passing the test suite). Thus, the CI bot enforces this. Please rebase your development history into a sensible list of self-contained commits that tell the story of your bug fix or improvement. Carve them up so that the riskier bits can be reverted independently. Keep changes focussed by splitting out cleanups from refactorings from actual changes to the logic.

This facilitates reviewing: a commit that reformats code can be judged quickly not to affect anything, so we can focus on the meat of the PR. It also helps when merging between long-running branches, reducing conflicts (or providing at least a limited scope for each one).

Please do not @-mention anyone in the commit message -- that's what the PR description and comments are for. Every time a commit is shuffled through github (in a merge in some fork, say), every @-mention results in an email to that person (the core team treats them as personal email, straight to their inbox, so please don't flood us :-)).


## Reviews

Please consider nominating a reviewer for your PR in the PR's description or a comment. If unsure, not to worry -- the core team will assign one for you.

Your reviewer is also your mentor, who will help you rework your PR so that it meets our requirements. We strive to give timely feedback, and apologize for those times when we are overwhelmed by the volume of contributions. Please feel free to ping us. You are entitled to regular progress updates and at least a quick assessment of feasibility of a bigger PR.

To help you plan your contributions, we communicate our plans on a regular basis on scala-internals, and deadlines are tracked as due dates for [GitHub milestones](https://github.com/scala/scala/milestones).

## Reviewing

Once you've gained some experience with the code base and the process, the next step is to review the contributions of others.

The main goal of this whole process is to ensure the health of the Scala project by improving the quality of the code base, the documentation, as well as this process itself. Thank you for doing your part!

## [Labels](https://github.com/scala/scala/labels)

Label           | Description
--------------- | -----------
`reviewed`      | automatically added by scabot when a comment prefixed with LGTM is posted
`welcome`       | reviewer / queue curator adds to welcome someone's first PR (for highlighting in the release notes)
`release-notes` | reviewer / queue curator adds to make sure this PR is highlighted in the release notes
`on-hold`       | added when this PR should not yet be merged, even though CI is green

### Tips & Tricks
Once the `publish-core` task has completed on a commit, you can try it out in sbt as follows:

```
$ sbt

> set resolvers += "pr" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/"
> set scalaVersion := "<milestone>-<sha7>-SNAPSHOT"
> console
```

Here, `<milestone>` is the milestone targeted by the PR (e.g., 2.11.6), and `<sha7>` is the 7-character sha (the format used by GitHub on the web).

## IDE Setup
### Eclipse
See [src/eclipse/README.md](src/eclipse/README.md).

### IntelliJ 15
See [src/intellij/README.md](src/intellij/README.md).

## Building with sbt (EXPERIMENTAL)

The experimental sbt-based build definition has arrived! Run `sbt package`
to build the compiler. You can run `sbt test` to run unit (JUnit) tests.
Use `sbt test/it:test` to run integration (partest) tests.

We would like to migrate to sbt build as quickly as possible. If you would
like to help please use the scala-internals mailing list to discuss your
ideas and coordinate your effort with others.

## Building with Ant

NOTE: we are working on migrating the build to sbt.

If you are behind a HTTP proxy, include
[`ANT_ARGS=-autoproxy`](https://ant.apache.org/manual/proxy.html) in
your environment.

Run `ant build-opt` to build an optimized version of the compiler.
Verify your build using `ant test-opt`.

The Scala build system is based on Apache Ant. Most required pre-compiled
libraries are part of the repository (in 'lib/'). The following however is
assumed to be installed on the build machine: TODO

### Ant Tips and tricks

Here are some common commands. Most ant targets offer a `-opt` variant that runs under `-optimise` (CI runs the -optimize variant).

Command                 | Description
----------------------- | -----------
`./pull-binary-libs.sh` | downloads all binary artifacts associated with this commit.
`ant -p`                | prints out information about the commonly used ant targets.
`ant` or `ant build`    | A quick compilation (to `build/quick`) of your changes using the locker compiler.
`ant dist`              | builds a distribution in 'dists/latest'.
`ant all.clean`         | removes all build files and all distributions.

A typical debug cycle incrementally builds quick, then uses it to compile and run the file
`sandbox/test.scala` as follows:

  - `ant && build/quick/bin/scalac -d sandbox sandbox/test.scala && build/quick/bin/scala -cp sandbox Test`

We typically alias `build/quick/bin/scalac -d sandbox` to `qsc` and `build/quick/bin/scala -cp sandbox` to `qs` in our shell.

`ant test-opt` tests that your code is working and fit to be committed:

  - Runs the test suite and bootstrapping test on quick.
  - You can run the suite only (skipping strap) with `ant test.suite`.

`ant docs` generates the HTML documentation for the library from the sources using the scaladoc tool in quick.
Note: on most machines this requires more heap than is allocated by default.  You can adjust the parameters with `ANT_OPTS`. Example command line:

```sh
ANT_OPTS="-Xms512M -Xmx2048M -Xss1M -XX:MaxPermSize=128M" ant docs
```

### Bootstrapping concepts
NOTE: This is somewhat outdated, but the ideas still hold.

In order to guarantee the bootstrapping of the Scala compiler, the ant build
compiles Scala in layers. Each layer is a complete compiled Scala compiler and library.
A superior layer is always compiled by the layer just below it. Here is a short
description of the four layers that the build uses, from bottom to top:

  - `starr`: the stable reference Scala release. We use an official version of Scala (specified by `starr.version` in `versions.properties`), downloaded from the Central Repository.
  - `locker`: the local reference which is compiled by starr and is the work compiler in a typical development cycle. Add `locker.skip=true` to `build.properties` to skip this step and speed up development when you're not changing code generation. In any case, after it has been built once, it is “frozen” in this state. Updating it to fit the current source code must be explicitly requested (`ant locker.unlock`).
  - `quick`: the layer which is incrementally built when testing changes in the compiler or library. This is considered an actual new version when locker is up-to-date in relation to the source code.
  - `strap`: a test layer used to check stability of the build.

For each layer, the Scala library is compiled first and the compiler next.
That means that any changes in the library can immediately be used in the
compiler without an intermediate build. On the other hand, if building the
library requires changes in the compiler, a new locker must be built if
bootstrapping is still possible, or a new starr if it is not.
