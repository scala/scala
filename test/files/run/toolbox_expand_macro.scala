import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox}

object Test extends App {
  val toolBox = cm.mkToolBox()
  val x = 21
  val runtimeMacro =
    q"""object RuntimeMacro {
      import scala.reflect.macros.whitebox.Context
      import scala.language.experimental.macros

      def add(y: Int): Int = macro addImpl
      def addImpl(c: Context)(y: c.Expr[Int]): c.Expr[Int] = {
        import c.universe._
        val x = $x
        c.Expr[Int](q"$$x + $$y")
      }
    }"""
  val s = toolBox.define(runtimeMacro)
  println(toolBox.eval(q"$s.add(21)"))
}
