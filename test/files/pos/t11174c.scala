trait CtorType
class Props extends CtorType {
  def foo(props: Int): Int = ???
}

object Generic {
  implicit def toComponentCtor[CT <: CtorType](c: ComponentSimple[CT]): CT = ???

  trait ComponentSimple[CT <: CtorType]
}

object Test {
  import Generic._

  val c: ComponentSimple[Props] = ???
  toComponentCtor(c).foo(23)
  c.foo(23)
}
