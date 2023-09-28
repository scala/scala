package repro

abstract class Boo {
  val b = Boo
}

// Tests simple class in nested objects
object Boo {
  object Foo {
    class Impl
  }
}

abstract class Boo2 {
  val b2 = Boo2
}

// Tests simple class in object, special-cased (by Scala)
// It does and should not trigger malformed class name.
object Boo2 {
  class Impl
}

abstract class Boo3 {
  val b3 = Boo3
}

// Tests three nested object + class
object Boo3 {
  object Foo2 {
    object Bar {
      class Impl
    }
  }
}

abstract class Boo4 {
  val b4 = Boo4
}

// Tests nested classes inside nested objects
object Boo4 {
  object Foo3 {
    class BarClass {
      class Impl
    }
  }
}
