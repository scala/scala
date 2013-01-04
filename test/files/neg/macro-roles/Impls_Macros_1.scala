import scala.reflect.macros.Context
import language.experimental.macros

class C extends scala.annotation.StaticAnnotation
class G[T]

object Macros {
  def impl(c: Context) = {
    val cc = c.asInstanceOf[scala.reflect.macros.runtime.Context]
    c.warning(c.enclosingPosition, s"app = ${c.macroApplication}, role = ${c.macroRole}")
    // println(cc.callsiteTyper.context.enclosingContextChain.map(_.tree))
  }

  def impl1(c: Context) = { impl(c); c.universe.Ident(c.universe.TypeName("C")) }
  def impl2(c: Context)() = { impl(c); c.universe.Ident(c.universe.TypeName("G")) }

  type Foo = macro impl1
  type Bar() = macro impl2
}