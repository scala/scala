/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools.nsc
package doc
package model

abstract sealed class LinkTo
final case class LinkToTpl(tpl: DocTemplateEntity) extends LinkTo
final case class LinkToMember(mbr: MemberEntity, inTpl: DocTemplateEntity) extends LinkTo
final case class Tooltip(name: String) extends LinkTo { def this(tpl: TemplateEntity) = this(tpl.qualifiedName) }
final case class LinkToExternal(name: String, url: String) extends LinkTo
case object NoLink extends LinkTo // you should use Tooltip if you have a name from the user, this is only in case all fails

object LinkToTpl {
  // this makes it easier to create links
  def apply(tpl: TemplateEntity) = tpl match {
    case dtpl: DocTemplateEntity => new LinkToTpl(dtpl)
    case ntpl: TemplateEntity => new Tooltip(ntpl.qualifiedName)
  }
}
