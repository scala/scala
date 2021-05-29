// scalac: -Xsource:3
import scala.tools.testkit.AssertUtil.assertThrown
import scala.util.chaining.*

case class CaseClass[A: Ordering](a: String, b: Int)(c: A)
case object CaseObject

object Test extends App {

  def check(t: Throwable)(msg: String)(ms: String*): Boolean =
    (t.getMessage == msg).tap(if (_) () else println(s"expected [$msg], got [${t.getMessage}]"))
    &&
    ms.forall(m => t.getStackTrace.exists(f => m == s"${f.getClassName}.${f.getMethodName}"))

  //java.lang.IndexOutOfBoundsException: 99
  assertThrown[IndexOutOfBoundsException](check(_)("99")("scala.runtime.Statics.ioobe", "CaseClass.productElementName")) {
    CaseClass("foo", 123)(42).productElementName(99)
  }
  assertThrown[IndexOutOfBoundsException](_ => true) {
    CaseClass("foo", 123)(42).productElementName(2)
  }
  //java.lang.IndexOutOfBoundsException: 99 is out of bounds (min 0, max -1
  assertThrown[IndexOutOfBoundsException](check(_)(s"99 is out of bounds (min 0, max -1)")("scala.Product.productElementName", "CaseObject$.productElementName")) {
    CaseObject.productElementName(99)
  }
}
