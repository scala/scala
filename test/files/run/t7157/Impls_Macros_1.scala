import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  object AImpl {
    def a(ctx: Context)(args: ctx.Expr[Any]*): ctx.Expr[Unit] = {
      import ctx.universe._
      ctx.Expr[Unit](Apply(Ident(TermName("println")), List(Literal(Constant(1)))))
    }
  }

  implicit class A(context: StringContext) {
    def a(args: Any*): Unit = macro AImpl.a
  }
}