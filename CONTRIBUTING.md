# Welcome! Thank you for contributing to Scala!
We follow the standard GitHub [fork & pull](https://help.github.com/articles/using-pull-requests/#fork--pull) approach to pull requests. Just fork the official repo, develop in a branch, and submit a PR!

You're always welcome to submit your PR straight away and start the discussion (without reading the rest of this wonderful doc, or the `READMEnot^H^H^H.md`). The goal of these notes is to make your experience contributing to Scala as smooth and pleasant as possible. We're happy to guide you through the process once you've submitted your PR.

## The Scala Community
Last year, you -- the Scala community -- matched the core team at EPFL in number of commits contributed to Scala 2.11, doubling the percentage of commits from outside EPFL/Typesafe since 2.10. Excellent work! (The split is roughly 25/25/50 for you/epfl/typesafe. By the way, the team at Typesafe is: @adriaanm, @gkossakowski, @lrytz and @retronym.)

We are super happy about this, and are eager to make your experience contributing to Scala productive and satisfying, so that we can keep up this growth. We can't do this alone (nor do we want to)!

This is why we're collecting these notes on how to contribute, and we hope you'll share your experience to improve the process for the next contributor! (Feel free to send a PR for this note, send your thoughts to scala-internals, or tweet about it to @adriaanm.)

## What kind of PR are you submitting?

Regardless of the nature of your Pull Request, we have to ask you to sign the [Scala CLA](http://typesafe.com/contribute/cla/scala), to protect the OSS nature of the code base.

### Documentation
Whether you finally decided you couldn't stand that annoying typo anymore, you fixed the outdated code sample in some comment, or you wrote a nice, comprehensive, overview for an under-documented package, some docs for a class or the specifics about a method, your documentation improvement is very much appreciated, and we will do our best to fasttrack it.

You can make these changes directly in your browser in GitHub, or follow the same process as for code. Up to you!

For bigger documentation changes, you may want to poll the (scala-internals) mailing list first, to quickly gauge whether others support the direction you're taking, so there won't be any surprises when it comes to reviewing your PR.

### Code
For bigger changes, we do recommend announcing your intentions on scala-internals first, to avoid duplicated effort, or spending a lot of time reworking something we are not able to change at this time in the release cycle, for example.

The kind of code we can accept depends on the life cycle for the release you're targeting. The current maintenance release (2.11.x) cannot break source/binary compatibility, which means public APIs cannot change. It also means we are reluctant to change, e.g., type inference or implicit search, as this can have unforeseen consequences for source compatibility.

#### Bug Fix

Prefix your commit title with "SI-NNNN", where https://issues.scala-lang.org/browse/SI-NNNN tracks the bug you're fixing. We also recommend naming your branch after the Jira ticket number.

Please make sure the Jira ticket's fix version corresponds to the upcoming milestone for the branch your PR targets (the CI automation will automatically assign the milestone after you open the PR).

#### Enhancement or New Feature

For longer-running development, likely required for this category of code contributions, we suggest you include "topic" or "wip" in your branch name, to indicate that this is work in progress, and that others should be prepared to rebase if they branch off your branch.

Any language change (including bug fixes) must be accompanied by the relevant updates to the spec, which lives in the same repository for this reason.

A new language feature requires a SIP (Scala Improvement Process) proposal. For more details on submitting SIPs, see [how to submit a SIP](http://docs.scala-lang.org/sips/sip-submission.html).

#### Summary

1. We require regression tests for bug fixes. New features and enhancements must be supported by a respectable test suite.
2. Documentation. Yep! Also required :-)
3. Please follow these standard code standards, though in moderation (scouts quickly learn to let sleeping dogs lie):
   - Not violate [DRY](http://programmer.97things.oreilly.com/wiki/index.php/Don%27t_Repeat_Yourself).
   - [Boy Scout Rule](http://programmer.97things.oreilly.com/wiki/index.php/The_Boy_Scout_Rule) should be applied.

Please also have a look at our [Pull Request Policy](https://github.com/scala/scala/wiki/Pull-Request-Policy), as well as the [Scala Hacker Guide](http://www.scala-lang.org/contribute/hacker-guide.html) by @xeno-by.

