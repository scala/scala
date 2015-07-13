# Welcome! Thank you for contributing to Scala!
We follow the standard GitHub [fork & pull](https://help.github.com/articles/using-pull-requests/#fork--pull) approach to pull requests. Just fork the official repo, develop in a branch, and submit a PR!

You're always welcome to submit your PR straight away and start the discussion (without reading the rest of this wonderful doc, or the `READMEnot^H^H^H.md`). The goal of these notes is to make your experience contributing to Scala as smooth and pleasant as possible. We're happy to guide you through the process once you've submitted your PR.

## The Scala Community
In 2014, you -- the Scala community -- matched the core team at EPFL in number of commits contributed to Scala 2.11, doubling the percentage of commits from outside EPFL/Typesafe since 2.10. Excellent work! (The split was roughly 25/25/50 for you/EPFL/Typesafe.)

We are super happy about this, and are eager to make your experience contributing to Scala productive and satisfying, so that we can keep up this growth. We can't do this alone (nor do we want to)!

This is why we're collecting these notes on how to contribute, and we hope you'll share your experience to improve the process for the next contributor! (Feel free to send a PR for this note, send your thoughts to scala-internals, or tweet about it to @adriaanm.)

By the way, the team at Typesafe is: @adriaanm, @lrytz, @retronym, and @SethTisue.

## What kind of PR are you submitting?

Regardless of the nature of your Pull Request, we have to ask you to digitally sign the [Scala CLA](http://typesafe.com/contribute/cla/scala), to protect the OSS nature of the code base.

You don't need to submit separate PRs for 2.11.x, 2.12.x, and 2.13.x. Any changes accepted on one of these branches will, in time, be merged into the later branches.

### Documentation
Whether you finally decided you couldn't stand that annoying typo anymore, you fixed the outdated code sample in some comment, or you wrote a nice, comprehensive, overview for an under-documented package, some docs for a class or the specifics about a method, your documentation improvement is very much appreciated, and we will do our best to fasttrack it.

You can make these changes directly in your browser in GitHub, or follow the same process as for code. Up to you!

For bigger documentation changes, you may want to poll the (scala-internals) mailing list first, to quickly gauge whether others support the direction you're taking, so there won't be any surprises when it comes to reviewing your PR.

### Code
For bigger changes, we do recommend announcing your intentions on scala-internals first, to avoid duplicated effort, or spending a lot of time reworking something we are not able to change at this time in the release cycle, for example.

The kind of code we can accept depends on the life cycle for the release you're targeting. The current maintenance release (2.11.x) cannot break source/binary compatibility, which means public APIs cannot change. It also means we are reluctant to change, e.g., type inference or implicit search, as this can have unforeseen consequences for source compatibility.

#### Bug Fix

Prefix your commit title with "SI-NNNN", where https://issues.scala-lang.org/browse/SI-NNNN tracks the bug you're fixing. We also recommend naming your branch after the JIRA ticket number.

Please make sure the JIRA ticket's fix version corresponds to the upcoming milestone for the branch your PR targets. The CI automation will automatically assign the milestone after you open the PR.

#### Enhancement or New Feature

For longer-running development, likely required for this category of code contributions, we suggest you include "topic" or "wip" in your branch name, to indicate that this is work in progress, and that others should be prepared to rebase if they branch off your branch.

Any language change (including bug fixes) must be accompanied by the relevant updates to the spec, which lives in the same repository for this reason.

A new language feature requires a SIP (Scala Improvement Process) proposal. For more details on submitting SIPs, see [how to submit a SIP](http://docs.scala-lang.org/sips/sip-submission.html).

#### Summary

1. We require regression tests for bug fixes. New features and enhancements must be supported by a respectable test suite.
   - Consider including comments in the test files that indicates what you're testing and why. Expected outcome, what happened before the fix, what happens now, that sort of thing.
2. Documentation. Yep! Also required :-)
3. Please follow these standard code standards, though in moderation (scouts quickly learn to let sleeping dogs lie):
   - Not violate [DRY](http://programmer.97things.oreilly.com/wiki/index.php/Don%27t_Repeat_Yourself).
   - [Boy Scout Rule](http://programmer.97things.oreilly.com/wiki/index.php/The_Boy_Scout_Rule) should be applied.

Please also have a look at our [Pull Request Policy](https://github.com/scala/scala/wiki/Pull-Request-Policy), as well as the [Scala Hacker Guide](http://www.scala-lang.org/contribute/hacker-guide.html) by @xeno-by.


# Pull Request Policy

taken from https://github.com/scala/scala/wiki/Pull-Request-Policy
before it was put out of its misery

Hi there, pull request submitter!

Your pull request should:
  - (... have been discussed on scala-internals)
  - merge cleanly
  - consist of commits with messages that clearly state which problem this commit resolves and how
    - it should be stated in the active, present tense
    - its subject should be 60 characters or less
    - [conventions](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
    - for a bug fix, the title must look like "SI-NNNN - don't crash when moon is in wrong phase"
    - overall, think of the first line of the commit as a description of the action performed by the commit on the code base, so use the present tense -- that also makes them easy to reuse in release notes
    - backports should be tagged as [backport], it's also nice to mention this when a commit purely refactors and is not intended to change behaviour
    - when working on maintenance branches (e.g., 2.10.x), include [nomerge] if this commit should not be merged forward into the next release branch
  - come with tests (included in the same commit as the functional change), or explain in detail why it cannot be tested (discuss this on scala-internals first). The tests itself should:
    - be minimal, deterministic, stable (unaffected by irrelevant changes), easy to understand and review
    - have minimal dependencies: a compiler bug should not depend on, e.g. the Scala library
    - typically fail before your fix is applied (so we see that you are fixing a legitimate bug) and should obviously pass after your fix
  - come with appropriate documentation
    - for example, any API additions should include Scaladoc
  - each commit must pass the test suite (checked automatically by the build bot by running approximately `ant test-opt`) 
     - a commit is considered a unit of useful change and must thus pass the test suite
       (this way we stand a chance running git bisect later)
  - be assigned to one or more reviewers (if you're not sure, see the list below or contact scala-internals)
     - to assign a reviewer, add a "review by @reviewer" to your PR description. NOTE: it's best not to @mention in commit messages, as github pings you every time a commit with your @name on it shuffles through the system (even in other repos, on merges,...)
  - get the green light from the reviewer ("LGTM" -- looks good to me)
     - review feedback may be addressed by pushing new commits to the request,
       if these commits stand on their own

Once all these conditions are met, and we agree with the change
(we are available on scala-internals to discuss this beforehand),
we will merge your changes.

Please note: you are responsible for meeting these criteria  (reminding your reviewer, for example).

### Pull request bot mechanics
* The build bot automatically builds commits as they appear in a PR. Click on the little x next to a commit sha to go to the overview of the PR validation job. To diagnose a failure, consult the console output of the job that failed.
  * 'PLS REBUILD ALL' will force the bot to rebuild (only necessary when a spurious failure occurred -- i.e., you expect a different validation outcome without changing the commit shas that make up the PR)

### List of reviewers by area:

* library: @phaller (Philipp Haller), @axel22 (Aleksander Prokopec -- concurrent & collection) 
* specialisation: @axel22 (Aleksander Prokopec), @vladureche (Vlad Ureche), @dragos (Iulian Dragos)
* named / default args, annotations, plugins: @lrytz (Lukas Rytz)
* macros, reflection, manifests, string interpolation: @xeno-by (Eugene Burmako), @cvogt (Christopher Vogt)
* type checker, inference: @odersky (Martin Odersky), @adriaanm (Adriaan Moors)
* Language specification, value classes: @odersky (Martin Odersky)
* new pattern matcher, implicit search: @adriaanm (Adriaan Moors)
* partest, Continuations Plugin: @phaller (Philipp Haller)
* error handling, lazy vals: @hubertp (Hubert Plociniczak)
* backend: @magarciaEPFL (Miguel Garcia), @gkossakowski (Grzegorz Kossakowski), @dragos (Iulian Dragos)
* repl, compiler performance: @retronym (Jason Zaugg)
* swing: @ingoem (Ingo Maier)
* scaladoc: @dickwall (Dick Wall)
* optimizer: @vladureche (Vlad Ureche), @magarciaEPFL (Miguel Garcia)
* build: @jsuereth (Josh Suereth)
* random compiler bugs: @lrytz, @adriaanm, @hubertp
* documentation: @heathermiller (Heather Miller), @dickwall (Dick Wall)
* cps: @TiarkRompf (Tiark Rompf)
