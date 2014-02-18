import scala.language.reflectiveCalls
import scala.language.implicitConversions

object Test extends App {
  implicit def doubleWithApproxEquals(d: Double) = new {
    def ~==(v: Double, margin: Double = 0.001): Boolean =
      math.abs(d - v) < margin
  }

  assert(math.abs(-4.0) ~== (4.0, 0.001))
  assert(math.abs(-4.0) ~== 4.0)
}
