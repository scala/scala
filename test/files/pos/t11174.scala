trait CtorType[P]
class Props[P] extends CtorType[P] {
  def foo(props: P): P = ???
}

object Generic {
  implicit def toComponentCtor[CT[p] <: CtorType[p]](c: ComponentSimple[CT]): CT[Int] = ???

  trait ComponentSimple[CT[p] <: CtorType[p]]
}

object Test {
  import Generic._

  val c: ComponentSimple[Props] = ???
  toComponentCtor(c).foo(23)
  c.foo(23)
}
