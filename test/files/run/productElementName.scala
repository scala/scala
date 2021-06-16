// scalac: -Xsource:3
import scala.tools.testkit.AssertUtil.assertThrown
import scala.util.chaining.*
import org.junit.Assert.assertEquals

case class User(name: String, age: Int)

case class ユーザ(名前: String, 年齢: Int)

case class U$er(na$me: String, a$ge: Int)

case class `type`(`for`: String, `if`: Int)

case class `contains spaces`(`first param`: String, `second param`: Int)

case class Symbols(:: : String, || : Int)

case class MultipleParamLists(a: String, b: Int)(c: Boolean)

case class AuxiliaryConstructor(a: String, b: Int) {
  def this(x: String) = this(x, 123)
}

case class OverloadedApply(a: String, b: Int)
object OverloadedApply {
  def apply(x: String): OverloadedApply = new OverloadedApply(x, 123)
}

case class DefinesProductElementName(a: String, b: Int) {
  override def productElementName(n: Int): String = "foo"
}

trait A {
  def productElementName(n: Int): String = "overridden"
}
case class InheritsProductElementName(a: String, b: Int) extends A

trait B extends Product {
  override def productElementName(n: Int): String = "overridden"
}
case class InheritsProductElementName_Override(a: String, b: Int) extends B

trait C { self: Product =>
  override def productElementName(n: Int): String = "overridden"
}
case class InheritsProductElementName_Override_SelfType(a: String, b: Int) extends C

case class PrivateMembers(a: Int, private val b: Int, c: Int, private val d: Int, e: Int, private val f: Int)

case class ImplicitParameter[A: Ordering](a: String, b: Int)(c: A)

case object CaseObject

object Test extends App {
  def verify(p: Product, checkName: Boolean = true): Unit = {
    val iterated = p.productElementNames.zip(p.productIterator)
                    .map { case (name, value) => s"$name=$value" }
                    .mkString(p.productPrefix + "(", ", ", ")")
    val indexed  = (0 until p.productArity)
                    .map(i => s"${p.productElementName(i)}=${p.productElement(i)}")
                    .mkString(p.productPrefix + "(", ", ", ")")
    assertEquals(iterated, indexed)
    if (checkName) assertThrown[IndexOutOfBoundsException](_ => true)(p.productElementName(p.productArity + 1))
    println(iterated)
  }

  verify(User("Susan", 42))
  verify(ユーザ("Susan", 42))
  verify(U$er("Susan", 42))
  verify(`type`("Susan", 42))
  verify(`contains spaces`("Susan", 42))
  verify(Symbols("Susan", 42))
  verify(MultipleParamLists("Susan", 42)(true))
  verify(AuxiliaryConstructor("Susan", 42))
  verify(OverloadedApply("Susan"))
  verify(DefinesProductElementName("Susan", 42), checkName = false)

  // uses the synthetic, not the one defined in the trait
  verify(InheritsProductElementName("Susan", 42))

  // uses the override defined in the trait
  verify(InheritsProductElementName_Override("Susan", 42), checkName = false)

  // uses the synthetic, not the one defined in the trait
  verify(InheritsProductElementName_Override_SelfType("Susan", 42))

  verify(PrivateMembers(10, 20, 30, 40, 50, 60))

  // message check and probe for characteristic stack frames
  def check(t: Throwable)(msg: String)(ms: String*): Boolean =
    (t.getMessage == msg).tap(if (_) () else println(s"expected [$msg], got [${t.getMessage}]"))
    &&
    ms.forall(m => t.getStackTrace.exists(f => m == s"${f.getClassName}.${f.getMethodName}"))

  //java.lang.IndexOutOfBoundsException: 99
  assertThrown[IndexOutOfBoundsException](check(_)("99")("scala.runtime.Statics.ioobe", "ImplicitParameter.productElementName")) {
    ImplicitParameter("foo", 123)(42).productElementName(99)
  }
  assertThrown[IndexOutOfBoundsException](_ => true) {
    ImplicitParameter("foo", 123)(42).productElementName(2)
  }
  //java.lang.IndexOutOfBoundsException: 99 is out of bounds (min 0, max -1 [sic]
  assertThrown[IndexOutOfBoundsException](check(_)(s"99 is out of bounds (min 0, max -1)")("scala.Product.productElementName", "CaseObject$.productElementName")) {
    CaseObject.productElementName(99)
  }
}
