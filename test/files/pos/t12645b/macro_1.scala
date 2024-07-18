
//> using options -Xsource:3

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait Ast
case class Foo(id: Int) extends Ast
object Foo {
  val ctx = new Ctx {}
}

trait Ctx {
  trait Quoted[+A] {
    def ast: Ast
  }
  final def quote[A](a: A): Quoted[A] = macro QuoteMacro.quoteImpl[A]
}

class QuoteMacro(val c: Context) {
  import c.universe.*

  def quoteImpl[A](a: c.Expr[A])(implicit t: WeakTypeTag[A]) =
    c.untypecheck {
      q"""
      new ${c.prefix}.Quoted[$t] {
        def ast = Foo(42)
      }
      """
    }
}
