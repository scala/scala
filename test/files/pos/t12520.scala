class Outcome
trait TestSuite {
  protected trait NoArgTest extends (() => Outcome)
  protected def withFixture(test: NoArgTest): Outcome = test()
}

trait TestSuiteMixin { this: TestSuite =>
  protected def withFixture(test: NoArgTest): Outcome
}

trait TimeLimitedTests extends TestSuiteMixin { this: TestSuite =>
  abstract override def withFixture(test: NoArgTest): Outcome = super.withFixture(test)
}

trait AnyFunSuiteLike extends TestSuite
abstract class Test[C] extends AnyFunSuiteLike with TimeLimitedTests