# Welcome! Thank you for contributing to Scala!

Last year, you -- the Scala community -- matched the core team at EPFL in number of commits contributed to Scala 2.11, doubling the percentage of commits from outside EPFL/Typesafe since 2.10. Excellent work!

We are super happy about this, and are eager to make your experience contributing to Scala productive and satisfying, so that we can keep up this growth. We can't do this alone (nor do we want to)!

This is why we're collecting these notes on how to contribute, and we hope you'll share your experience to improve the process for the next contributor! (Feel free to send a PR for this note, send your thoughts to scala-internals, or tweet about it to @adriaanm.)

## What kind of PR are you submitting?

Regardless of the nature of your Pull Request, we have to ask you to sign the [Scala CLA](http://typesafe.com/contribute/cla/scala), to protect the OSS nature of the code base.

### Documentation
Whether you finally decided you couldn't stand that annoying typo anymore, you fixed the outdated code sample in some comment, or you wrote a nice, comprehensive, overview for an under-documented package, some docs for a class or the specifics about a method, your documentation improvement is very much appreciated, and we will do our best to fasttrack it.

You can make these changes directly in your browser in GitHub, or follow the same process as for code. Up to you!

For bigger documentation changes, you may want to poll the (scala-internals) mailing list first, to quickly gauge whether others support the direction you're taking, so there won't be any surprises when it comes to reviewing your PR.

### Code

You're always welcome to submit a PR straight away, although we recommend announcing your intentions on scala-internals first, to avoid duplicated effort, or spending a lot of time reworking something we are not able to change at this time in the release cycle, for example.

To reduce friction, we use the [fork & pull](https://help.github.com/articles/using-pull-requests/#fork--pull) approach to pull requests: you fork the official repository, develop your pull request in a branch in your fork, and send us a request to have your work pulled into the official fork (i.e., a Pull Request).

The kind of code we can accept depends on the life cycle for the release you're targeting. The current maintenance release (2.11.x) cannot break source/binary compatibility, which means public APIs cannot change. It also means we are reluctanct to change, e.g., type inference or implicit search, as this can have unforeseen consequences for source compatibility.

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


## Git Hygiene

As git history is forever, we take great pride in the quality of the commits we merge into the repository. The title of your commit will be read hundreds (of thousands? :-)) of times, so it pays off to spend just a little bit more time to polish it, making it descriptive and concise. Please take a minute to read the advice [most projects agree on](https://github.com/erlang/otp/wiki/Writing-good-commit-messages), and stick to 50-60 characters for the first line, wrapping subsequent ones at 80 (at most).

When not sure how to formulate your commit message, imagine you're writing a bullet item for the next release notes, or describing what the commit does to the code base (use active verbs in the present tense). When your commit title is featured in the next release notes, it will be read by a lot of curious Scala users, looking for the latest improvements. Satisfy their thirst for information with as few words as possible! Also, a commit should convey clearly to your (future) fellow contributors what it does to the code base.

Writing the commit message is a great sanity check that the commit is of the right size. If it does too many things, the description will be unwieldy and tedious to write. Chop it up (`git add -u --patch` and `git rebase` are your friends) and simplify!

To pinpoint bugs, we often use git bisect, which is only effective when we can count on each commit building (and passing the test suite). Thus, the CI bot enforces this. Please rebase your development history into a sensible list of self-contained commits that tell the story of your bug fix or improvement. Carve them up so that the riskier bits can be reverted independently. Keep changes focussed by splitting out cleanups from refactorings from actual changes to the logic.

This facilitates reviewing: a commit that reformats code can be judged quickly not to affect anything, so we can focus on the meat of the PR. It also helps when merging between long-running branches, reducing conflicts (or providing at least a limited scope for each one).

Please do not @mention anyone in the commit message -- that's what the PR description and comments are for. Every time a commit is shuffled through github (in a merge in some fork, say), every @mention results in an email to that person (the core team treats them as personal email, straight to their inbox, so please don't flood us :-)).


## Reviewing

Please consider nominating a reviewer for your PR in the PR's description or a comment. If unsure, not to worry -- the core team will assign one for you.

Your reviewer is also your mentor, who will help you rework your PR so that it meets our requirements. We strive to give timely feedback, and apologize for those times when we are overwhelmed by the volume of contributions. Please feel free to ping us. You are entitled to regular progress updates and at least a quick assessment of feasibility of a bigger PR.

To help you plan your contributions, we communicate our plans on a regular basis on scala-internals, and deadlines are tracked as due dates for [GitHub milestones](https://github.com/scala/scala/milestones).

Once you've established some history submitting PRs, we will invite you to become a reviewer for others's code. The main goal of this whole process, in the end, is to ensure the health of the Scala project by improving the quality of the code base, the documentation, as well as this process itself. Thank you for doing your part!