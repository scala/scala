class A(val a: Int = 13)
class DefaultsTest(x: Int = 25) extends A(28)
object DefaultsTest extends DefaultsTest(12)

object Test extends App {
  println(new DefaultsTest().a)
  println(DefaultsTest.a)
}
