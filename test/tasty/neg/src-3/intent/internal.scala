package tastytest.intent

trait TestLanguage {
  def expect[T](expr: => T)(given pos: Position): Expect[T] = new Expect[T](expr, pos)
}

trait Eq[T]

object IntEq extends Eq[Int]

trait EqGivens with
  given Eq[Int] = IntEq

trait Formatter[T]

object IntFmt extends Formatter[Int]

trait FormatterGivens with
  given Formatter[Int] = IntFmt

abstract class TestSuite
trait Expectation

trait Stateless extends IntentStatelessSyntax with ExpectGivens with EqGivens with FormatterGivens

class Expect[T](blk: => T, position: Position, negated: Boolean = false)

trait ExpectGivens with

  def [T](expect: Expect[T]) toEqual (expected: T)(given eqq: Eq[T], fmt: Formatter[T]): Expectation = ???

trait IntentStatelessSyntax extends TestLanguage with

  def (testName: String) in (testImpl: => Expectation)(given pos: Position): Unit = ???
  def (blockName: String) apply (block: => Unit)(given pos: Position): Unit = ???
