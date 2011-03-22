/* NSC -- new Scala compiler -- Copyright 2007-2011 LAMP/EPFL */

package scala.tools.nsc
package doc
package model
package comment

import scala.collection._

import java.net.URL

/** A body of text. A comment has a single body, which is composed of at least one block. Inside every body is exactly
  * one summary (see [[scala.tools.nsc.doc.model.comment.Summary]]). */
final case class Body(blocks: Seq[Block]) {

  /** The summary text of the comment body. */
  lazy val summary: Option[Inline] = {
    def summaryInBlock(block: Block): Seq[Inline] = block match {
      case Title(text, _)        => summaryInInline(text)
      case Paragraph(text)       => summaryInInline(text)
      case UnorderedList(items)  => items flatMap { summaryInBlock(_) }
      case OrderedList(items, _) => items flatMap { summaryInBlock(_) }
      case DefinitionList(items) => items.values.toSeq flatMap { summaryInBlock(_) }
      case _                     => Nil
    }
    def summaryInInline(text: Inline): Seq[Inline] = text match {
      case Summary(text)     => List(text)
      case Chain(items)      => items flatMap { summaryInInline(_) }
      case Italic(text)      => summaryInInline(text)
      case Bold(text)        => summaryInInline(text)
      case Underline(text)   => summaryInInline(text)
      case Superscript(text) => summaryInInline(text)
      case Subscript(text)   => summaryInInline(text)
      case Link(_, title)    => summaryInInline(title)
      case _                 => Nil
    }
    (blocks flatMap { summaryInBlock(_) }).toList match {
      case Nil => None
      case inline :: Nil => Some(inline)
      case inlines => Some(Chain(inlines))
    }
  }

}

/** A block-level element of text, such as a paragraph or code block. */
sealed abstract class Block

final case class Title(text: Inline, level: Int) extends Block
final case class Paragraph(text: Inline) extends Block
final case class Code(data: String) extends Block
final case class UnorderedList(items: Seq[Block]) extends Block
final case class OrderedList(items: Seq[Block], style: String) extends Block
final case class DefinitionList(items: SortedMap[Inline, Block]) extends Block
final case class HorizontalRule() extends Block

/** An section of text inside a block, possibly with formatting. */
sealed abstract class Inline

final case class Chain(items: Seq[Inline]) extends Inline
final case class Italic(text: Inline) extends Inline
final case class Bold(text: Inline) extends Inline
final case class Underline(text: Inline) extends Inline
final case class Superscript(text: Inline) extends Inline
final case class Subscript(text: Inline) extends Inline
final case class Link(target: String, title: Inline) extends Inline
final case class EntityLink(target: TemplateEntity) extends Inline
final case class Monospace(text: String) extends Inline
final case class Text(text: String) extends Inline
final case class HtmlTag(data: String) extends Inline {
  def canClose(open: HtmlTag) = {
    open.data.stripPrefix("<") == data.stripPrefix("</")
  }

  def close = {
    if (data.indexOf("</") == -1)
      Some(HtmlTag("</" + data.stripPrefix("<")))
    else
      None
  }
}

/** The summary of a comment, usually its first sentence. There must be exactly one summary per body. */
final case class Summary(text: Inline) extends Inline
