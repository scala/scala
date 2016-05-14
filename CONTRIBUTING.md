# Welcome! Thank you for contributing to Scala!
We follow the standard GitHub [fork & pull](https://help.github.com/articles/using-pull-requests/#fork--pull) approach to pull requests. Just fork the official repo, develop in a branch, and submit a PR!

You're always welcome to submit your PR straight away and start the discussion (without reading the rest of this wonderful doc, or the `READMEnot^H^H^H.md`). The goal of these notes is to make your experience contributing to Scala as smooth and pleasant as possible. We're happy to guide you through the process once you've submitted your PR.

## The Scala Community
In 2014, you -- the Scala community -- matched the core team at EPFL in number of commits contributed to Scala 2.11, doubling the percentage of commits from outside EPFL/Lightbend since 2.10. Excellent work! (The split was roughly 25/25/50 for you/EPFL/Lightbend.)

We are super happy about this, and are eager to make your experience contributing to Scala productive and satisfying, so that we can keep up this growth. We can't do this alone (nor do we want to)!

This is why we're collecting these notes on how to contribute, and we hope you'll share your experience to improve the process for the next contributor! (Feel free to send a PR for this note, send your thoughts to gitter, scala-internals, or tweet about it to @adriaanm.)

By the way, the team at Lightbend is: @adriaanm, @lrytz, @retronym, @SethTisue, and @szeiger.

## What kind of PR are you submitting?

Regardless of the nature of your Pull Request, we have to ask you to digitally sign the [Scala CLA](http://www.lightbend.com/contribute/cla/scala), to protect the OSS nature of the code base.

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

For longer-running development, likely required for this category of code contributions, we suggest you include "topic/" or "wip/" in your branch name, to indicate that this is work in progress, and that others should be prepared to rebase if they branch off your branch.

Any language change (including bug fixes) must be accompanied by the relevant updates to the spec, which lives in the same repository for this reason.

A new language feature requires a SIP (Scala Improvement Process) proposal. For more details on submitting SIPs, see [how to submit a SIP](http://docs.scala-lang.org/sips/sip-submission.html).

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

### Documentation

This is of course required for new features and enhancements.

Any API additions should include Scaladoc.

Consider updating the package-level doc (in the package object), if appropriate.

### Coding standards

Please follow these standard code standards, though in moderation (scouts quickly learn to let sleeping dogs lie):

* Don't violate [DRY](http://programmer.97things.oreilly.com/wiki/index.php/Don%27t_Repeat_Yourself).
* Follow the [Boy Scout Rule](http://programmer.97things.oreilly.com/wiki/index.php/The_Boy_Scout_Rule).

Please also have a look at the [Scala Hacker Guide](http://www.scala-lang.org/contribute/hacker-guide.html) by @xeno-by.

### Clean commits, clean history

A pull request should consist of commits with messages that clearly state what problem the commit resolves and how.

Commit logs should be stated in the active, present tense.

A commit's subject should be 72 characters or less.  Overall, think of
the first line of the commit as a description of the action performed
by the commit on the code base, so use the active voice and the
present tense.  That also makes the commit subjects easy to reuse in
release notes.

For a bugfix, the title must look like "SI-NNNN - don't crash when
moon is in wrong phase".

If a commit purely refactors and is not intended to change behaviour,
say so.

Backports should be tagged as "[backport]".

When working on maintenance branches (e.g., 2.11.x), include "[nomerge]"
if this commit should not be merged forward into the next release
branch.

Here is standard advice on good commit messages:
http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

### Pass Scabot

Our pull request bot, Scabot, automatically builds all the commits in a PR individually. (All, so we can `git bisect` later.)

Click on the little x next to a commit sha to go to the overview of the PR validation job. To diagnose a failure, consult the console output of the job that failed.

See the [scala-jenkins-infra repo](https://github.com/scala/scala-jenkins-infra) and [Scabot repo](https://github.com/scala/scabot) for full details on PR validation.  One tip you should know is that commenting `/rebuild` on a PR asks validation to be run again on the same commits. This is only necessary when a spurious failure occurred.

### Pass code review

Your PR will need to be assigned to one or more reviewers. You can suggest reviewers
yourself; if you're not sure, see the list in [README.md](README.md) or ask on gitter
or scala-internals.

To assign a reviewer, add a "review by @reviewer" to the PR description or in a
comment on your PR.

NOTE: it's best not to @mention in commit messages, as github pings you every time a commit with your @name on it shuffles through the system (even in other repos, on merges,...).

A reviewer gives the green light by commenting "LGTM" (looks good to me).

When including review feedback, we typically amend the changes into the existing commit(s)
and `push -f` to the branch. This is to keep the git history clean. Additional commits
are OK if they stand on their own.

Once all these conditions are met, and we agree with the change (we are available on
gitter or scala-internals to discuss this beforehand, before you put in the coding work!),
we will merge your changes.

We use the following labels:

Label                    | Description
-------------------------|:-----------
`reviewed`               | automatically added by scabot when a comment prefixed with LGTM is posted
`welcome`                | added by reviewer / queue curator to welcome someone's first PR (for highlighting in the release notes)
`release-notes`          | added by reviewer / queue curator to make sure this PR is highlighted in the release notes
`on-hold`                | added when this PR should not yet be merged, even though CI is green
`WIP`                    | added by the author if a PR is submitted for CI testing, needs more work to be complete
`assistance-appreciated` | added by the author if help by the community is appreciated to move a change forward
