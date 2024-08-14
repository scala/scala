//> using options -Wconf:cat=lint-missing-interpolator:ws,cat=deprecation:ws -Werror -Xlint

class C {
  def f: Unit = 42

  def oops = "$f"

  @deprecated("old stuff", since="1.0")
  def old = 17

  def stale = old
}
