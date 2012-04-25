/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api
import language.implicitConversions

trait Exprs { self: Universe =>

  /** An expression tree tagged with its type */
  case class Expr[+T: TypeTag](tree: Tree) {
    def tpe: Type = implicitly[TypeTag[T]].tpe
    def eval: T = mkToolBox().runExpr(tree).asInstanceOf[T]
    lazy val value: T = eval
    override def toString = "Expr["+tpe+"]("+tree+")"
  }
}