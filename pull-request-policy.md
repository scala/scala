---
layout: default
title: Pull Request Policy
---

Hi there, pull request submitter!

Your pull request should:
  - (... have been discussed on scala-internals)
  - merge cleanly
  - consist of commits with impeccable commit messages
    - [conventions](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
    - for a bug fix, the title must look like "SI-NNNN - don't crash when moon is in wrong phase"
    - overall, think of the first line of the commit as a description of the action performed by the commit on the code base, so use the present tense -- that also makes them easy to reuse in release notes
    - backports should be tagged as [backport], it's also nice to mention this when a commit purely refactors and is not intended to change behaviour
  - come with tests (included in the same commit as the functional change), or explain in detail why it cannot be tested (discuss this on scala-internals first). The tests itself should:
    - be minimal, deterministic, stable (unaffected by irrelevant changes), easy to understand and review
    - have minimal dependencies: a compiler bug should not depend on, e.g. the Scala library
    - typically fail before your fix is applied (so we see that you are fixing a legitimate bug) and should obviously pass after your fix
  - pass the test suite (checked automatically by the build bot, run `ant test-opt` locally to verify)
     - if the build fails, the request will be closed without further ado
       you may not push new commits to fix the problem
       a commit is considered a unit of useful change and must thus pass the test suite
       (this way we stand a chance running git bisect later)
  - be assigned to one or more reviewers (if you're not sure, see the list below or contact scala-internals)
  - get the green light from the reviewer ("LGTM" -- looks good to me)
     - review feedback may be addressed by pushing new commits to the request,
       if these commits stand on their own

Once all these conditions are met, and we agree with the change
(we are available on scala-internals to discuss this beforehand),
we will merge your changes.

Please note: you are responsible for meeting these criteria  (reminding your reviewer, for example).

### Pull request bot mechanics
* most of this will be automated, but in the mean time, please consider the following:
  * when you push new commits, please delete the comments with the old build results to avoid confusion (this will also trigger a new build, even though the bot detects this automatically)
  * label the issue corresponding to your pull request (if you have permission) as 'tested' when the bot has reported on a successful build, 'reviewed' when everyone has LGTM'd
  * 'PLS REBUILD ALL' will force the bot to rebuild (handy for spurious failures)

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
* repl, compiler performance: @paulp (Paul Phillips)
* swing: @ingoem (Ingo Maier)
* scaladoc: @vladureche (Vlad Ureche)
* optimizer: @vladureche (Vlad Ureche), @magarciaEPFL (Miguel Garcia)
* build: @jsuereth (Josh Suereth)
* random compiler bugs: @lrytz, @adriaanm, @hubertp
* documentation: @heathermiller (Heather Miller)
* cps: @TiarkRompf (Tiark Rompf)
