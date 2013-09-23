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

## Testing

All bug-fixes and any non-trivial changes should come with tests.

Scala project has at the moment two main ways of writing tests:

  1. Partest tests located in `test/files`
  2. JUnit tests located in `test/junit`

Partest is a homegrown testing framework designed for high-level, [functional tests](http://en.wikipedia.org/wiki/Functional_testing). Partest is designed specifically to test Scala compiler and tools surrounding it (like REPL). It achieves that by defining various categories of tests. For example, tests in `test/files/pos` check whether given piece of code compiles (pos stands for "positive"). There are cases where we want to check that given code doesn't compile (e.g. doesn't type check) so we have `test/files/neg` for that. If you want to see the complete list of supported test categories run `test/partest --help` command. Check existing tests in each category to see how write one yourself.

[JUnit](http://junit.org/) tests are [unit tests](http://en.wikipedia.org/wiki/Unit_testing) based on well-known, Java testing framework. Unit tests are meant to test single unit of functionality like a single class that performs a well-defined job. Example of unit tests (in context of Scala project) include:

  * Scala library tests. E.g. all collection tests that assert certain behavior should be written as JUnit tests
  * Symbol table tests that check correctness and contracts of Trees, Symbols and Types that do not require the whole compile to run (e.g. subtyping can be tested as an unit test)
  * Single component of Scala compiler can and should be unit tested. For example, if you are writing an optimizer then it's a good idea to test each optimization in separation by passing some inputs (bytecode instructions) and asserting properties of outputs.

In principle, every unit test can be written as partest's `run` test but there are several advantages of writing those tests as JUnit tests:

  * JUnit has native IDE support so you can easily run and debug unit test in your favorite IDE
  * JUnit tests use proper package structure (tests follow package structure of classes they test) so all tests can be compiled at once which dramatically speeds up compilation of tests
  * JUnit reuses the same JVM instance to run all tests which speeds up tests execution

Scala project has grown to the size where performance of test execution started to matter a lot. Please take that into account when you are writing your tests.

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
For more details on submitting SIPs, see (how to submit a SIP)[http://docs.scala-lang.org/sips/sip-submission.html].
