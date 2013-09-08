import scala.language._

trait R[+Repr]

trait TraversableOps {
  implicit val R: R[Nothing] = ???

  // Removing the implicit parameter in both fixes the crash
  // removing it into one only gives a valid compiler error.
  trait OpsDup1[Repr] {
    def force(implicit bf: R[Repr]): Any
  }

  trait Ops[Repr] extends OpsDup1[Repr] {
    def force(implicit bf: R[Repr], dummy: DummyImplicit): Any
  }

  implicit def ct2ops[T, C[+X]](t: C[T]):
    Ops[C[T]]

  def force[T](t: Option[T]) =
    // ct2ops(t).force
    t.force //Fails compilation on 2.10.2.


  /* To get a closer look at the crash:
  :power
  val foo = typeOf[C].member(TermName("foo"))
  val pt = analyzer.HasMember(TermName("force"))
  val instantiated = foo.info.finalResultType.instantiateTypeParams(foo.typeParams, foo.typeParams.map(TypeVar(_)))
  instantiated <:< pt
  */
  def foo[T, C[+X]]: Ops[C[T]]
}
