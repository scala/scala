/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.makro
package runtime

trait Reifiers {
  self: Context =>

  import mirror._
  import definitions._

  lazy val reflectMirrorPrefix: Tree = ???

  def reifyTree(prefix: Tree, tree: Tree): Tree =
    scala.reflect.reify.`package`.reifyTree(mirror)(callsiteTyper, prefix, tree)

  def reifyType(prefix: Tree, tpe: Type, dontSpliceAtTopLevel: Boolean = false, concrete: Boolean = false): Tree =
    scala.reflect.reify.`package`.reifyType(mirror)(callsiteTyper, prefix, tpe, dontSpliceAtTopLevel, concrete)

  def reifyRuntimeClass(tpe: Type, concrete: Boolean = true): Tree =
    scala.reflect.reify.`package`.reifyRuntimeClass(mirror)(callsiteTyper, tpe, concrete)

  def unreifyTree(tree: Tree): Tree =
    Select(tree, definitions.ExprSplice)
}
