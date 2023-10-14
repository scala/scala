import scala.annotation.nowarn
import scala.language.reflectiveCalls
import scala.language.implicitConversions

object Test extends App {
  @nowarn  // the inferred type includes the default arg, which can't be written explicitly
  implicit def doubleWithApproxEquals(d: Double) = new {
    def ~==(v: Double, margin: Double = 0.001): Boolean =
      math.abs(d - v) < margin
  }

  assert(math.abs(-4.0) ~== (4.0, 0.001))
  assert(math.abs(-4.0) ~== 4.0)
}
/*
was:
Exception in thread "main" java.lang.IllegalAccessError: tried to access field illegal_access_error_test_case$.reflParams$Cache2 from class illegal_access_error_test_case$delayedInit$body
  at illegal_access_error_test_case$delayedInit$body.<clinit>(illegal_access_error_test_case.scala:8)
  at illegal_access_error_test_case$.<init>(illegal_access_error_test_case.scala:1)
  at illegal_access_error_test_case$.<clinit>(illegal_access_error_test_case.scala)
  at illegal_access_error_test_case.main(illegal_access_error_test_case.scala)
 */
