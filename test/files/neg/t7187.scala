//> using options -Xlint:deprecation,eta-zero,eta-sam
//

trait AcciSamOne { def apply(x: Int): Int }

@FunctionalInterface
trait SamOne { def apply(x: Int): Int }

class EtaExpandZeroArg {
  def foo(): () => String = () => ""
  val t1a: () => Any = foo() // ok (obviously)
  val t1b: () => Any = foo   // eta-expansion, but lint warning
  val t1c: () => Any = { val t = foo; t } // `()`-insertion because no expected type
  val t1d: () => Any = foo _ // ok, explicit eta-expansion requested
  val t1e: Any       = foo _ // ok, explicit eta-expansion requested
  val t1f: Any       = foo() _ // error: _ must follow method

  def bar = ""
  val t2a: () => Any = bar   // error: no eta-expansion of zero-arglist-methods (nullary methods)
  val t2b: () => Any = bar() // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument
  val t2c: () => Any = bar _ // warning: eta-expanding a nullary method
  val t2d: Any       = bar _ // warning: eta-expanding a nullary method
  val t2e: Any       = bar() _ // error: not enough arguments for method apply

  def baz() = ""
  val t3a: () => Any = baz   // eta-expansion, but lint warning
  val t3b: () => Any = baz _ // ok
  val t3c: Any       = baz _ // ok
  val t3d: Any       = baz() _ // error: _ must follow method

  def zap()() = ""
  val t4a: () => Any = zap     // eta-expansion, but lint warning
  val t4b: () => Any = zap()   // ditto
  val t4c: () => Any = zap _   // ok
  val t4d: () => Any = zap() _ // ok

  def zup(x: Int) = x
  val t5a = zup // error in 2.13, eta-expansion in 3.0
  val t5Fun: Int => Int = zup // ok
  val t5AcciSam: AcciSamOne = zup // ok, but warning
  val t5Sam: SamOne = zup // ok
}
