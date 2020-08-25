// scalac: -deprecation -Xfatal-warnings
class EtaExpandZeroArg {
  def foo(): () => String = () => ""
  val t1a: () => Any = foo() // ok (obviously)
  val t1b: () => Any = foo   // eta-expansion (deprecated) in 2.12, `()`-insertion in 2.13
  val t1c: () => Any = { val t = foo; t } // ok, no expected type, `()`-insertion
  val t1d: () => Any = foo _ // ok
  val t1e: Any       = foo _ // ok
  val t1f: Any       = foo() _ // error: _ must follow method

  def bar = ""
  val t2a: () => Any = bar   // error: no eta-expansion of zero-arglist-methods
  val t2b: () => Any = bar() // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument
  val t2c: () => Any = bar _ // ok
  val t2d: Any       = bar _ // ok
  val t2e: Any       = bar() _ // error: not enough arguments for method apply

  def baz() = ""
  val t3a: () => Any = baz   // eta-expansion (deprecated) in 2.12, error in 2.13
  val t3b: () => Any = baz _ // ok
  val t3c: Any       = baz _ // ok
  val t3d: Any       = baz() _ // error: _ must follow method

  def zap()() = ""
  val t4a: () => Any = zap     // eta-expansion (deprecated) in 2.12, error in 2.13
  val t4b: () => Any = zap()   // ditto
  val t4c: () => Any = zap _   // ok
  val t4d: () => Any = zap() _ // ok
}
