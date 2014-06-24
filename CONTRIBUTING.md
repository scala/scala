# Scala Project & Developer Guidelines

These guidelines are meant to be a living document that should be changed and adapted as needed. We encourage changes that make it easier to achieve our goals in an efficient way.

## General Workflow

This is the process for committing code to the Scala project. There are of course exceptions to these rules, for example minor changes to comments and documentation, fixing a broken build etc.

1. Make sure you have signed the [Scala CLA](http://typesafe.com/contribute/cla/scala), if not, sign it.
2. Before starting to work on a feature or a fix, it's good practice to ensure that:
    1. There is a ticket for your work in the project's issue tracker. If not, create it first (perhaps given a thumbs up from the scala-internals mailing list first).
    2. The ticket has been discussed and prioritized by the team.
3. You should always perform your work in its own Git branch. The branch should be given a descriptive name that explains its intent. Some teams also like adding the ticket number and/or the [GitHub](http://github.com) user ID to the branch name, these details is up to each of the individual teams. (See below for more details on branch naming.)
4. When the feature or fix is completed you should open a [Pull Request](https://help.github.com/articles/using-pull-requests) on GitHub.
5. The Pull Request should be reviewed by other maintainers (as many as feasible/practical). Note that a reviewer can also be an outside contributor-- members of Typesafe and independent contributors are encouraged to participate in the review process. It is not a closed process. Please try to avoid conflict of interest -- the spirit of the review process is to evenly distribute the understanding of our code base across its maintainers as well as to load balance quality assurance. Assigning a review to a "sure win" reviewer is not a good long-term solution.
6. After the review, you should resolve issues brought up by the reviewers as needed (pushing a new commit to address reviewers' comments), iterating until the reviewers give their thumbs up, the "LGTM" (acronym for "Looks Good To Me").
7. Once the code has passed review the Pull Request can be merged into the distribution.

## Pull Request Requirements

First, please have a look at and follow the [Pull Request Policy](https://github.com/scala/scala/wiki/Pull-Request-Policy) for guidelines on submitting a pull request to the Scala project.

In order for a Pull Request to be considered, it has to meet these requirements:

1. Live up to the current code standard:
   - Not violate [DRY](http://programmer.97things.oreilly.com/wiki/index.php/Don%27t_Repeat_Yourself).
   - [Boy Scout Rule](http://programmer.97things.oreilly.com/wiki/index.php/The_Boy_Scout_Rule) should be applied.
2. Tests are of paramount importance.
3. The code must be well documented in the project's standard documentation format (see the ‘Documentation’ section below).

If *all* of these requirements are not met then the code should **not** be merged into the distribution, and need not even be reviewed.

## Documentation

All contributed code should come accompanied with documentation. Pull requests containing undocumented code will not be accepted. Both user-facing Scaladoc comments, as well as committer-facing internal documentation (i.e. important design decisions that other maintainers should know about should be placed inline with line comments `//`) should be accompanying all contributed code where possible.


## Work In Progress

It is ok to work on a public feature branch in the GitHub repository. Something that can sometimes be useful for early feedback etc. If so, then it is preferable to name the branch accordingly. This can be done by either prefixing the name with ``wip-`` as in ‘Work In Progress’, or use hierarchical names like ``wip/..``, ``feature/..`` or ``topic/..``. Either way is fine as long as it is clear that it is work in progress and not ready for merge. This work can temporarily have a lower standard. However, to be merged into master it will have to go through the regular process outlined above, with Pull Request, review etc..

Also, to facilitate both well-formed commits and working together, the ``wip`` and ``feature``/``topic`` identifiers also have special meaning.   Any branch labelled with ``wip`` is considered “git-unstable” and may be rebased and have its history rewritten.   Any branch with ``feature``/``topic`` in the name is considered “stable” enough for others to depend on when a group is working on a feature.

## Creating Commits And Writing Commit Messages

Follow these guidelines when creating public commits and writing commit messages.

1. If your work spans multiple local commits (for example; if you do safe point commits while working in a feature branch or work in a branch for long time doing merges/rebases etc.) then please do not commit it all but rewrite the history by squashing the commits into one large commit which is accompanied by a detailed commit message for (as discussed in the following sections). For more info, see the article: [Git Workflow](http://sandofsky.com/blog/git-workflow.html). Additionally, every commit should be able to be used in isolation-- that is, each commit must build and pass all tests.
2. The first line should be a descriptive sentence about what the commit is doing. It should be possible to fully understand what the commit does by just reading this single line. It is **not ok** to only list the ticket number, type "minor fix" or similar. If the commit has a corresponding ticket, include a reference to the ticket number, prefixed with "SI-", at the beginning of the first line followed by the title of the ticket, assuming that it aptly and concisely summarizes the commit in a single line. If the commit is a small fix, then you are done. If not, go to 3.
3. Following the single line description (ideally no more than 70 characters long) should be a blank line followed by an enumerated list with the details of the commit.
4. Add keywords for your commit (depending on the degree of automation we reach, the list may change over time):
    * ``Review by @githubuser`` - will notify the reviewer via GitHub. Everyone is encouraged to give feedback, however. (Remember that @-mentions will result in notifications also when pushing to a WIP branch, so please only include this in your commit message when you're ready for your pull request to be reviewed. Alternatively, you may request a review in the pull request's description.)
    * ``Fix/Fixing/Fixes/Close/Closing/Refs #ticket`` - if you want to mark the ticket as fixed in the issue tracker (Assembla understands this).
    * ``backport to _branch name_`` - if the fix needs to be cherry-picked to another branch (like 2.9.x, 2.10.x, etc)

Example:

    SI-4032 Implicit conversion visibility affected by presence of "this"

      - Details 1
      - Details 2
      - Details 3

## The Scala Improvement Process
A new language feature requires a SIP (Scala Improvement Process) proposal. Note that significant additions to the standard library are also considered candidates for a SIP proposal.
For more details on submitting SIPs, see [how to submit a SIP](http://docs.scala-lang.org/sips/sip-submission.html).

# Development workflow FAQ

There are some useful practices to follow when working on this repository. If there's something not in this FAQ, please don't hesitate to ask [scala-internals](https://groups.google.com/forum/#!forum/scala-internals) mailing list.

## Workflow 101: building, running tests, etc.

The project is built with `ant`. There are several stages of the compilation: locker, quick, pack, strap, and docs. Most of the time when working on this repository, you would only want to perform the quick stage.

**Q:** How do I build the repo?

**A:** `ant build`
***
**Q:** How do I clean-build the repo?

**A:** `ant clean build`
***
**Q:** How do I clean the repo, including things that got compiled during the locker stage?

**A:** `ant all.clean`
***
**Q:** Is it possible to not compile locker at all, and use a released scala version for it?

**A:** Yes. You need to have a `build.properties` file in the root of this repo, which contains:
```
locker.skip=1
starr.use.released=1
```
***
**Q:** How do I run tests?

**A:** `ant test`
***
**Q:** Can I use some kind of `grep`-like interface for deciding which tests to run?

**A:** Yes you can! `./tools/partest-ack "dottype"`
***
**Q:** So, I've built myself a scala version. How do I use it now?

**A:** `/build/quick/bin/` contains `scala`, `scalac`, `scaladoc`, and `scalap` executables.
***
**Q:** Can I try and use this newly built scala from a local sbt project?

**A:** You can. Here's how:
```
$ ant publish-local-opt -Dmaven.version.suffix="-test"
$ sbt
[info] Set current project to test (in build file:/Users/georgii/workspace/test/)
> set resolvers += Resolver.mavenLocal
[info] Defining *:resolvers
[info] The new value will be used by *:externalResolvers
[info] Reapplying settings...
[info] Set current project to test (in build file:/Users/georgii/workspace/test/)
> ++2.12.0-test
[info] Setting version to 2.12.0-test
[info] Set current project to test (in build file:/Users/georgii/workspace/test/)
> console
[info] Starting scala interpreter...
[info]
Welcome to Scala version 2.12.0-20140623-155543-8bdacad317 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_51).
Type in expressions to have them evaluated.
Type :help for more information.

scala>
```

## Workflow 102: how do I do this, and what did just happen

Some things that you want to perform might be not that straightforward to achieve. Here are some:

**Q:** How do I add a macro to `Predef`?

**A:** For the reason of bootstrapping, you cannot just throw a macro into the `Predef`. There is a more involved process here. You might want to follow the way `StringContext.f` is added. In short, you need to define your macro under `src/compiler/scala/tools/reflect/` and provide no implementation in `Predef` (`def fn = macro ???`). Now you want to set up the wiring. Add the name of your macro to `src/reflect/scala/reflect/internal/StdNames.scala`, add the needed links to it to `src/reflect/scala/reflect/internal/Definitions.scala`, and finally specify the bindings in `src/compiler/scala/tools/reflect/FastTrack.scala`. [Here's](https://github.com/folone/scala/commit/59536ea833ca16c985339727baed5d70e577b0fe) an example of adding a macro.
***
**Q:** Can I use quasiquotes in the compiler?

**A:** No, you cannot, for the same reason of bootstrapping. You will have to build trees by hand. Hint: to get an idea of what kind of tree your quasiquote produces, you might want to use `showRaw`.
***
**Q:** What's the story with tests?

**A:** Tests are run with a tool called [partest](https://github.com/scala/scala-partest). You can run them all with `ant test`, or manually with `test/partest` executable. There are several categories of tests. Most notable are `scalacheck`, `run`, and `neg`. `scalacheck` tests are property-based tests. `run` and `neg` are both interaction test-suites (with the distinction that `neg` tests are expected to not compile). That means compiling/running those tests will produce input-output interaction that is captured in the `.check` files (expected output). You will get test failures if the content of a `.check` file is different from what the test produces while running. If the change in the output is an expected product of your work, you might not want to change the `.check` file by hand. To make partest change the `.check` file, run it with a `--update-check` flag, like so `./test/partest --update-check path/to/test.scala`. For more information on partest, please refer to its [documentation](http://docs.scala-lang.org/tutorials/partest-guide.html).
***
**Q:** I just tried to run tests, and they failed with a strange-looking error. Something like:
```
test.bc:
     [echo] Checking backward binary compatibility for scala-library (against 2.11.0)
     [mima] Found 2 binary incompatibiities
     [mima] ================================
     [mima]  * synthetic method
     [mima]    scala$package$Class$method(java.lang.String)Unit in trait
     [mima]    scala.package.Class does not have a correspondent in old version
     [mima]  * synthetic method
     [mima]    scala$package$AnotherClass$anotherMethod(java.lang.String)Unit in trait
     [mima]    scala.package.AnotherClass does not have a correspondent in old version
     [mima] Generated filter config definition
     [mima] ==================================
     [mima]
     [mima]     filter {
     [mima]         problems=[
     [mima]             {
     [mima]                 matchName="scala.package.Class$method"
     [mima]                 problemName=MissingMethodProblem
     [mima]             },
     [mima]             {
     [mima]                 matchName="scala.package.AnotherClass$anotherMethod"
     [mima]                 problemName=MissingMethodProblem
     [mima]             }
     [mima]         ]
     [mima]     }
     [mima]

BUILD FAILED
/localhome/jenkins/c/workspace/pr-scala-test/scala/build.xml:1530: The following error occurred while executing this line:
/localhome/jenkins/c/workspace/pr-scala-test/scala/build-ant-macros.xml:791: The following error occurred while executing this line:
/localhome/jenkins/c/workspace/pr-scala-test/scala/build-ant-macros.xml:773: Java returned: 2

Total time: 6 minutes 46 seconds
Build step 'Execute shell' marked build as failure
Archiving artifacts
Notifying upstream projects of job completion
Finished: FAILURE
```
What does this mean and what do I do now?

**A:** This means your change is backward or forward binary incompatible with the specified version (the check is performed by the [migration manager](https://github.com/typesafehub/migration-manager)). The error message is actually saying what you need to add to `bincompat-backward.whitelist.conf` or `bincompat-forward.whitelist.conf` to make the error go away. If you are getting this on an internal/experimental api, it should be safe to add suggested sections to the config. Otherwise, you might want to target a newer version of scala for this change.
