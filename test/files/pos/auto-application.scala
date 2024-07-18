//> using options -Werror -deprecation -Xsource:3
//
class Test {
  def a1(xs: List[String]): Int = xs.hashCode
  def a2(xs: List[String]): Int = xs.hashCode()
  def a3(xs: List[String]): String = xs.toString
  def a4(xs: List[String]): String = xs.toString()
  def a5(xs: List[String]): Class[_] = xs.getClass
  def a6(xs: List[String]): Class[_] = xs.getClass()
  def a7(xs: List[String]): Int = xs.##
  def a9(x: Address): String = x.toString
  def a10(x: Address): String = x.toString()
  def a11(x: A): String = x.toString
  def a12(x: A): String = x.toString()
  def a13(x: B): String = x.toString
  def a14(x: B): String = x.toString()
}

case class Address()

class A() {
  override def toString(): String = "A()"
}

class B() {
  override def toString: String = "B()"
}

// Value class generates this.underlying.hashCode
case class C(c: Int) extends AnyVal

// This generates toString$extension
class Bibbity(val i: Int) extends AnyVal {
  override def toString = "hi"
}

class City extends Runnable { override def run(): Unit = () }
object City {
  val c = new City
  c.run // should be ok without parens
}

object Sam {
  val r: java.lang.Runnable = () => ()
  r.run // should be ok without parens
}
