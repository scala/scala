/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait Exprs { self: Universe =>

  /** An expression tree tagged with its type */
  case class Expr[+T: TypeTag](tree: Tree) {
    def tpe: Type = implicitly[TypeTag[T]].tpe
    def eval: T = mkToolBox().runExpr(tree).asInstanceOf[T]
    lazy val value: T = eval
    override def toString = "Expr["+tpe+"]("+tree+")"
  }

  // [Eugene] had to move this to the companion of Tree to make stuff compile. weirdo!
//  object Expr {
//    // would be great if in future this generated an Expr[Magic]
//    // where Magic is a magic untyped type that propagates through the entire quasiquote
//    // and turns off typechecking whenever it's involved
//    // that'd allow us to splice trees into quasiquotes and still have these qqs to be partially typechecked
//    // see some exploration of these ideas here: https://github.com/xeno-by/alphakeplerdemo
//    implicit def tree2expr(tree: Tree): Expr[Nothing] = Expr[Nothing](tree)
//    implicit def expr2tree(expr: Expr[_]): Tree = expr.tree
//
//    // [Eugene] good idea?
//    implicit def trees2exprs(trees: List[Tree]): List[Expr[Nothing]] = trees map tree2expr
//    implicit def exprs2trees(exprs: List[Expr[_]]): List[Tree] = exprs map expr2tree
//  }

  // [Eugene] even weirder - implicits didn't feel at home in Trees :(

  // would be great if in future this generated an Expr[Magic]
  // where Magic is a magic untyped type that propagates through the entire quasiquote
  // and turns off typechecking whenever it's involved
  // that'd allow us to splice trees into quasiquotes and still have these qqs to be partially typechecked
  // see some exploration of these ideas here: https://github.com/xeno-by/alphakeplerdemo
  implicit def tree2expr(tree: Tree): Expr[Nothing] = Expr[Nothing](tree)(TypeTag.Nothing)
  implicit def expr2tree(expr: Expr[_]): Tree = expr.tree

  // [Eugene] good idea?
  implicit def trees2exprs(trees: List[Tree]): List[Expr[Nothing]] = trees map tree2expr
  implicit def exprs2trees(exprs: List[Expr[_]]): List[Tree] = exprs map expr2tree
}

