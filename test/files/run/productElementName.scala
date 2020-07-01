
case class User(name: String, age: Int)

case class ユーザ(名前: String, 年齢: Int)

case class U$er(na$me: String, a$ge: Int)

case class `type`(`for`: String, `if`: Int)

case class `contains spaces`(`first param`: String, `second param`: Int)

case class Symbols(:: : String, || : Int)

case class MultipleParamLists(a: String, b: Int)(c: Boolean)

case class AuxiliaryConstructor(a: String, b: Int) {
  def this(x: String) = {
    this(x, 123)
  }
}

case class OverloadedApply(a: String, b: Int)
object OverloadedApply {
  def apply(x: String): OverloadedApply =
    new OverloadedApply(x, 123)
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

object Test extends App {
  def pretty(p: Product): String =
    p.productElementNames.zip(p.productIterator)
     .map { case (name, value) => s"$name=$value" }
     .mkString(p.productPrefix + "(", ", ", ")")

  println(pretty(User("Susan", 42)))
  println(pretty(ユーザ("Susan", 42)))
  println(pretty(U$er("Susan", 42)))
  println(pretty(`type`("Susan", 42)))
  println(pretty(`contains spaces`("Susan", 42)))
  println(pretty(Symbols("Susan", 42)))
  println(pretty(MultipleParamLists("Susan", 42)(true)))
  println(pretty(AuxiliaryConstructor("Susan", 42)))
  println(pretty(OverloadedApply("Susan")))
  println(pretty(DefinesProductElementName("Susan", 42)))

  // uses the synthetic, not the one defined in the trait
  println(pretty(InheritsProductElementName("Susan", 42)))

  // uses the override defined in the trait
  println(pretty(InheritsProductElementName_Override("Susan", 42)))

  // uses the synthetic, not the one defined in the trait
  println(pretty(InheritsProductElementName_Override_SelfType("Susan", 42)))

  println(pretty(PrivateMembers(10, 20, 30, 40, 50, 60)))
}

