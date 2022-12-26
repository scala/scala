// scalac: -Xsource:3
class Test
trait A {
  val instance: Test = new Test
}

trait B extends A {
  override object instance extends Test
}

object Test extends App {
  println(new B {})
}
