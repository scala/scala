//> using options -nowarn -Wconf:cat=lint-missing-interpolator:ws -Werror -Xlint -Xsource:3

/*
-nowarn and -Xlint are in contradiction. Which wins?
-nowarn is recognized by per-run reporting and by sink reporter (e.g., ConsoleReporter).
For per-run reporting, -nowarn means default wconf for deprecation must not be ws (summary, which would warn).
Instead, it is w or s depending on whether -deprecation is requested.
So from the perspective of per-run reporting, -deprecation means issue a diagnostic even if -nowarn.

For the sink reporter, too, -nowarn means "don't summarize with warning count".
In addition, -nowarn means -maxwarns:0, so any warning is filtered by FilteringReporter.
(Normally, displayed warnings is capped by -maxwarns and then summarized as a count when done.)
So from the perspective of the sink reporter, -nowarn means filter out warnings and don't print count of warnings.
It doesn't consider -deprecation at all.
In addition, -nowarn subverts -Werror.

In the test example, there are 2 lints, a non-lint, and a migration warning.
The wconf boosts a lint to ws summary, but -nowarn tells the sink not to print either a warning or a count.
Migration is boosted to e by default, but -nowarn says don't boost migration warnings.
A user-supplied wconf could boost migration despite -nowarn.
Other warnings are silenced by any:s installed by -nowarn.
*/
trait T {
  def g[A]: Option[A]
}

class C extends T {
  def f: Unit = 42 // suppressed other warning for expr, lint for parens

  override def g[A] = None // suppressed migration warning, not boosted to error under --no-warnings

  def oops = "$f" // summarized lint

  @deprecated("old stuff", since="1.0")
  def old = 17

  def stale = old
}
