trait Alias {
  def foo = {
    type A
  }
  val bar = {
    type B
    object O {
      type OK
    }
  }
}

class C {
  def this(a: Any) = { this(); type C }
}
