/* NSC -- new Scala compiler
 * Copyright 2007-2016 LAMP/EPFL
 * @author David Bernard, Manohar Jonnalagedda, Felix Mulder
 */

package scala
package tools
package nsc
package doc
package html
package page

import base._
import base.comment._

import scala.reflect.internal.Reporter
import scala.collection.mutable
import scala.language.postfixOps

import model._
import model.diagram._
import diagram._

trait EntityPage extends HtmlPage {
  import ScalaDoc.SummaryReporter

  def universe: doc.Universe
  def generator: DiagramGenerator
  def tpl: DocTemplateEntity
  def docletReporter: Reporter

  override val path = templateToPath(tpl)

  def title = {
    val s = universe.settings
    ( if (!s.doctitle.isDefault) s.doctitle.value + " " else "" ) +
    ( if (!s.docversion.isDefault) s.docversion.value else "" ) +
    ( if ((!s.doctitle.isDefault || !s.docversion.isDefault) && tpl.qualifiedName != "_root_") " - " + tpl.qualifiedName else "" )
  }

  val valueMembers =
    tpl.methods ++ tpl.values ++ tpl.templates.filter(x => x.isObject) sorted

  val (absValueMembers, nonAbsValueMembers) =
    valueMembers partition (_.isAbstract)

  val (deprValueMembers, nonDeprValueMembers) =
    nonAbsValueMembers partition (_.deprecation.isDefined)

  val (concValueMembers, shadowedImplicitMembers) =
    nonDeprValueMembers partition (!_.isShadowedOrAmbiguousImplicit)

  val typeMembers =
    tpl.abstractTypes ++ tpl.aliasTypes ++ tpl.templates.filter(x => x.isTrait || x.isClass) sorted (implicitly[Ordering[MemberEntity]])

  val constructors = (tpl match {
    case cls: Class => (cls.constructors: List[MemberEntity]).sorted
    case _ => Nil
  })

  def memberToShortCommentTitleTag(mbr: MemberEntity): String =
    mbr.comment.fold("")(comment => Page.inlineToStrForTitleTag(comment.short))


  def visibility(mbr: MemberEntity): Option[comment.Paragraph] = {
    import comment._
    import comment.{ Text => CText }
    mbr.visibility match {
      case PrivateInInstance() =>
        Some(Paragraph(CText("private[this]")))
      case PrivateInTemplate(owner) if (owner == mbr.inTemplate) =>
        Some(Paragraph(CText("private")))
      case PrivateInTemplate(owner) =>
        Some(Paragraph(Chain(List(CText("private["), EntityLink(comment.Text(owner.qualifiedName), LinkToTpl(owner)), CText("]")))))
      case ProtectedInInstance() =>
        Some(Paragraph(CText("protected[this]")))
      case ProtectedInTemplate(owner) if (owner == mbr.inTemplate) =>
        Some(Paragraph(CText("protected")))
      case ProtectedInTemplate(owner) =>
        Some(Paragraph(Chain(List(CText("protected["), EntityLink(comment.Text(owner.qualifiedName), LinkToTpl(owner)), CText("]")))))
      case Public() =>
        None
    }
  }

  private def bodyToStr(body: comment.Body): String =
    body.blocks flatMap (blockToStr(_)) mkString ""

  private def blockToStr(block: comment.Block): String = block match {
    case comment.Paragraph(in) => Page.inlineToStr(in)
    case _ => block.toString
  }
}

object EntityPage {
  def apply(
    uni: doc.Universe,
    gen: DiagramGenerator,
    docTpl: DocTemplateEntity,
    rep: Reporter
  ): EntityPage = new EntityPage {
    def universe = uni
    def generator = gen
    def tpl = docTpl
    def docletReporter = rep
  }

  /* Vlad: Lesson learned the hard way: don't put any stateful code that references the model here,
   * it won't be garbage collected and you'll end up filling the heap with garbage */
  def lowerFirstLetter(s: String) = if (s.length >= 1) s.substring(0,1).toLowerCase() + s.substring(1) else s
}
