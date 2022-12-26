
class Test

class A {
  lazy val instance: Test = new Test
}

class B extends A {
  override object instance extends Test
}

object Test extends App {
  println(new B)
}

// error: value must be lazy when overriding concrete lazy value:
