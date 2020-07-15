package tastytest.intent

trait TestLanguage {
  def expect[T](expr: => T)(using pos: Position): Expect[T] = new Expect[T](expr, pos)
}

trait Eq[T]

object IntEq extends Eq[Int]

trait EqGivens:
  given Eq[Int] = IntEq

trait Formatter[T]

object IntFmt extends Formatter[Int]

trait FormatterGivens:
  given Formatter[Int] = IntFmt

abstract class TestSuite
trait Expectation

trait Stateless extends IntentStatelessSyntax with ExpectGivens with EqGivens with FormatterGivens

class Expect[T](blk: => T, position: Position, negated: Boolean = false)

trait ExpectGivens:

  def [T](expect: Expect[T]) toEqual (expected: T)(using eqq: Eq[T], fmt: Formatter[T]): Expectation = ???

trait IntentStatelessSyntax extends TestLanguage:

  def (testName: String) in (testImpl: => Expectation)(using pos: Position): Unit = ???
  def (blockName: String) apply (block: => Unit)(using pos: Position): Unit = ???
