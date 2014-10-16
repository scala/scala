/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.reify
package utils

trait StdAttachments {
  self: Utils =>

  import global._

  case class ReifyBindingAttachment(binding: Tree)

  def reifyBinding(tree: Tree): Tree =
    tree.attachments.get[ReifyBindingAttachment] match {
      case Some(ReifyBindingAttachment(binding)) => binding
      case other => Ident(NoSymbol)
    }

  case class ReifyAliasAttachment(sym: Symbol, alias: TermName)
}