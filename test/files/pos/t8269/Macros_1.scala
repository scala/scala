import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    Try(
      Literal(Constant(())),
      CaseDef(
        Bind(
          TermName("signal"),
          Typed(
            Ident(nme.WILDCARD),
            Ident(TypeName("Exception")))),
        EmptyTree,
        EmptyTree) :: Nil,
      EmptyTree)
  }

  def foo: Unit = macro impl
}
