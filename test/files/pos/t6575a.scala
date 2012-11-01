trait X { def foo: PartialFunction[Int, Int] }

trait Y extends X {
  // Inferred type was AbstractPartialFunction[Int, Int] with Serializable
  abstract override def foo = { case i => super.foo(i) * 2 }
}
trait Z extends X {
  // ditto
  abstract override def foo = { case i => super.foo(i) + 3 }
}

trait Comb extends Y with Z {
  // ... which led to a type error here.
  abstract override def foo: PartialFunction[Int, Int] = { case i => super.foo(i) - 2 }
}
