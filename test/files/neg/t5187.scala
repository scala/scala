
class Test

class A {
  object instance extends Test
}

class B extends A {
  override lazy val instance: Test = new Test
}

object Test extends App {
  println(new B)
}
