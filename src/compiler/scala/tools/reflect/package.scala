/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools

import scala.reflect.api.JavaUniverse
import language.implicitConversions

package object reflect extends FrontEnds {
  // [todo: can we generalize this?
  import scala.reflect.runtime.{universe => ru}
  implicit def ToolBox(mirror0: ru.Mirror): ToolBoxFactory[ru.type] =
    new ToolBoxFactory[ru.type](mirror0.universe) {
      lazy val mirror = mirror0
    }

  // todo. replace this with an implicit class, once the pesky warning is gone
  implicit def Eval[T](expr: JavaUniverse # Expr[T]): Eval[T] = new Eval[T](expr)

  // we don't provide `Eval` for trees, because it's unclear where to get an evaluation mirror from
}

package reflect {
  class Eval[T](expr: JavaUniverse # Expr[T]) {
    def eval: T = {
      val factory = new ToolBoxFactory[JavaUniverse](expr.mirror.universe) { val mirror = expr.mirror.asInstanceOf[this.u.Mirror] }
      val toolBox = factory.mkToolBox()
      toolBox.runExpr(expr.tree.asInstanceOf[toolBox.u.Tree]).asInstanceOf[T]
    }
  }
}
