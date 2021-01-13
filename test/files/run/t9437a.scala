
import java.lang.reflect.Parameter
import scala.reflect.{ClassTag, classTag}
import scala.reflect.NameTransformer

class C(a: Int, `_`: String, *** : Long, `unary_!` : Float, ABC: Double, `scala.lang`: Boolean, `a/b`: Boolean) {
  def f(a: Int, `_`: String, *** : Long, `unary_!` : Float, ABC: Double, `scala.lang`: Boolean, `a/b`: Boolean) = 42
}
class D
object D {
  def f(a: Int, `_`: String, *** : Long, `unary_!` : Float, ABC: Double, `scala.lang`: Boolean, `a/b`: Boolean) = 42
}

object Test extends App {
  def constrParams[A: ClassTag] = classTag[A].runtimeClass.getConstructors.head.getParameters
  def methodParams[A: ClassTag] = classTag[A].runtimeClass.getDeclaredMethods.head.getParameters

  def verifyParams(expected: Seq[String])(params: Array[Parameter]) =
    expected.zip(params).foreach {
      case (expect, actual) =>
        assert(actual.isNamePresent, s"name $expect should be present")
        assert(!actual.isSynthetic, s"name $expect should not be synthetic")
        val encoded = NameTransformer.encode(expect)
        assert(encoded == actual.getName, s"expected name $expect ($encoded) but was ${actual.getName}")
    }

  val expected = List("a", "_", "***", "unary_!", "ABC", "scala.lang", "a/b")
  verifyParams(expected)(constrParams[C])
  verifyParams(expected)(methodParams[C])
  verifyParams(expected)(methodParams[D])

  val c = new C(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0, `scala.lang` = true, `a/b` = false)
  val res = c.f(a = 1, `_` = "2", *** = 3L, `unary_!` = 4.0f, ABC = 5.0, `scala.lang` = true, `a/b` = false)
  assert(res == 42, s"bad result $res")
}
