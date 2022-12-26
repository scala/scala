// scalac: -Xsource:3
trait A {
  trait Test
  val instance: Test = new Test { }
}

class B extends A {
  override object instance extends Test
}

object Test extends App {
  println(new B)
}
