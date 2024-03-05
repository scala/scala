
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

// with unused interpolator check in typer, the variable and literal must be typechecked together to warn
object Macro {
  def id[A](body: A): A = macro impl[A]

  def impl[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]) = {
    import c.universe._
    q"""val unusedVariable = "42".toInt; println("hello, world of $$unusedVariable"); $body"""
  }
}
