package pkg {
  import scala.reflect.macros.blackbox.Context
  import scala.language.experimental.macros

  object Macros {
    def impl[T: c.WeakTypeTag](c: Context) = {
      import c.universe._
      val sym = c.weakTypeOf[T].typeSymbol
      val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
      c.Expr[Boolean](Literal(Constant(g.currentRun.compiles(sym.asInstanceOf[g.Symbol]))))
    }
    def compiles[T]: Boolean = macro impl[T]
  }
}

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

object Test extends App {
  val cm = ru.runtimeMirror(getClass.getClassLoader)
  val toolbox = cm.mkToolBox()
  toolbox.eval(toolbox.parse("""{
    class C
    println(pkg.Macros.compiles[C])
    println(pkg.Macros.compiles[Object])
  }"""))
}