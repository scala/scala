package foo.bar

class Outer {
  object InnerO {
    class A
    object B
  }
  class InnerC {
    trait T
  }
}

object Outer {
  class X {
    object Y
  }
}
