/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.partest
package utils

import scala.reflect.Code
import reflect.runtime.Mirror.ToolBox
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings

/** Runner for testing code tree liftingg
 */
object CodeTest {
  def static[T](code: () => T,  args: Array[String] = Array()) = {
    println("static: "+code())
  }

  def apply[T](code: Code[T], args: Array[String] = Array()) = {
    println("testing: "+code.tree)
    println("type is: "+code.manifest.tpe)
    val isNullary = code.manifest.tpe.typeSymbol == scala.reflect.mirror.definitions.FunctionClass(0)
    val reporter = new ConsoleReporter(new Settings)
    val toolbox = new ToolBox(reporter, args mkString " ")
    val ttree = toolbox.typeCheck(code.tree, code.manifest.tpe)
    println("result = " + toolbox.showAttributed(ttree, printTypes = true, printIds = false))
    var evaluated = toolbox.runExpr(ttree)
    if (evaluated != null && isNullary) {
      val applyMeth = evaluated.getClass.getMethod("apply")
      evaluated = applyMeth.invoke(evaluated)
    }
    println("evaluated = "+evaluated)
    evaluated
  }
}
