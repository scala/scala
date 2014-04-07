/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.reflect.internal.HasFlags
import scala.reflect.internal.Flags._
import symtab._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo extends scala.reflect.internal.TreeInfo {
  val global: Global
  import global._

  import definitions.ThrowableClass

  // TODO these overrides, and the slow trickle of bugs that they solve (e.g. SI-8479),
  //      suggest that we should pursue an alternative design in which the DocDef nodes
  //      are eliminated from the tree before typer, and instead are modelled as tree
  //      attachments.

  /** Is tree legal as a member definition of an interface?
   */
  override def isInterfaceMember(tree: Tree): Boolean = tree match {
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case _ => super.isInterfaceMember(tree)
  }

  override def isConstructorWithDefault(t: Tree) = t match {
    case DocDef(_, definition) => isConstructorWithDefault(definition)
    case _ => super.isConstructorWithDefault(t)
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  override def isPureDef(tree: Tree): Boolean = tree match {
    case DocDef(_, definition) => isPureDef(definition)
    case _ => super.isPureDef(tree)
  }

 /** Does list of trees start with a definition of
   *  a class of module with given name (ignoring imports)
   */
  override def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
    case ClassDef(_, `name`, _, _) :: Nil => true
    case _ => super.firstDefinesClassOrObject(trees, name)
  }

  def isInterface(mods: HasFlags, body: List[Tree]) =
    mods.isTrait && (body forall isInterfaceMember)
}
