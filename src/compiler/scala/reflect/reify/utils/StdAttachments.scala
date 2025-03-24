/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
      case _ => Ident(NoSymbol)
    }

  case class ReifyAliasAttachment(sym: Symbol, alias: TermName)
}
