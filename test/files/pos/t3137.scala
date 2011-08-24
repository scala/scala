trait A {
  val C: Any
}

class B extends A {
  class C
  object C
}

trait AA {
  type C
  def C: Int => C
}

class BB extends AA {
  case class C(v: Int)
}
