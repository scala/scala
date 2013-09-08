object Test {
  val c: reflect.macros.Context = ???
  import c.universe._
  reify {
    // The lookup of the implicit WeakTypeTag[Any]
    // was triggering an unpositioned tree.
    c.Expr[Any](Literal(Constant(0))).splice
  }

  import scala.reflect.ClassTag
  def ct[A: ClassTag]: Expr[A] = ???
  def tt[A: TypeTag]: Expr[A] = ???
  def wtt[A: WeakTypeTag]: Expr[A] = ???

  reify {
    ct[String].splice
    tt[String].splice
    wtt[String].splice
  }
}
