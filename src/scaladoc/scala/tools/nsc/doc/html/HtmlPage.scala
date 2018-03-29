/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala
package tools
package nsc
package doc
package html

import base._
import base.comment._
import model._

import scala.reflect.internal.Reporter
import scala.collection._
import java.io.Writer
import java.net.URI

/** An html page that is part of a Scaladoc site.
  * @author David Bernard
  * @author Gilles Dubochet */
abstract class HtmlPage extends Page { thisPage =>
  /** The title of this page. */
  protected def title: String

  /** ScalaDoc reporter for error handling */
  protected def docletReporter: Reporter

  /** The page description */
  protected def description: String =
    // unless overwritten, will display the title in a spaced format, keeping - and .
    title.replaceAll("[^a-zA-Z0-9\\.\\-]+", " ").replaceAll("\\-+", " - ").replaceAll(" +", " ")

  /** The page keywords */
  protected def keywords: String =
    // unless overwritten, same as description, minus the " - "
    description.replaceAll(" - ", " ")

  def writeFor(site: HtmlFactory) {
    writeFile(site) { (w: Writer) =>
      w.write('\n')
    }

    if (site.universe.settings.docRawOutput)
      writeFile(site, ".raw") {
        // we're only interested in the body, as this will go into the diff
        _.write('\n')
      }
  }

  def listItemsToHtml(items: Seq[Block]) = Seq.empty[Block]


  def hasPage(e: DocTemplateEntity) = {
    e.isPackage || e.isTrait || e.isClass || e.isObject || e.isCaseClass
  }

  def docEntityImageClass(tpl: DocTemplateEntity): String =
    tpl.kind + tpl.companion.fold("")("-companion-" + _.kind)

  def docEntityKindToCompanionTitle(ety: DocTemplateEntity, baseString: String = "See companion") =
    ety.companion match{
          case Some(companion) =>
	    s"$baseString${
		if(companion.isObject) " object"
		else if(companion.isTrait) " trait"
		else if(companion.isClass) " class"
		else ""
		}"
	  case None => baseString
	}

  private def memberToUrl(template: Entity, isSelf: Boolean = true): String = {
    val (signature: Option[String], containingTemplate: TemplateEntity) = template match {
      case dte: DocTemplateEntity if (!isSelf) => (Some(dte.signature), dte.inTemplate)
      case dte: DocTemplateEntity => (None, dte)
      case me: MemberEntity => (Some(me.signature), me.inTemplate)
      case tpl => (None, tpl)
    }

    val templatePath = templateToPath(containingTemplate)
    val url = "../" * (templatePath.size - 1) + templatePath.reverse.mkString("/")
    url + signature.map("#" + _).getOrElse("")
  }
}
