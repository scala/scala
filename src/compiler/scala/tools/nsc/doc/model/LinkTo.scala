/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 */

package scala.tools.nsc
package doc
package model

import scala.collection._

sealed trait LinkTo
final case class LinkToMember[Mbr, Tpl](mbr: Mbr, tpl: Tpl) extends LinkTo
final case class LinkToTpl[Tpl](tpl: Tpl) extends LinkTo
final case class LinkToExternal(name: String, url: String) extends LinkTo
final case class Tooltip(name: String) extends LinkTo

object LinkToTpl {
  // this makes it easier to create links
  def apply(tpl: TemplateEntity): LinkTo = tpl match {
    case dtpl: DocTemplateEntity => new LinkToTpl(dtpl)
    case _ => new Tooltip(tpl.qualifiedName)
  }
}
