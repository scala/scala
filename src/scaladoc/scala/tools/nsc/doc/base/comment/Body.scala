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

package scala.tools.nsc
package doc
package base
package comment

import scala.collection.immutable.SortedMap

/** A body of text. A comment has a single body, which is composed of
  * at least one block. Inside every body is exactly one summary.
  * @see [[Summary]]
  */
final case class Body(blocks: Seq[Block]) {

  /** The summary text of the comment body. */
  lazy val summary: Option[Inline] = {
    def summaryInBlock(block: Block): Seq[Inline] = block match {
      case Title(text, _)        => summaryInInline(text)
      case Paragraph(text)       => summaryInInline(text)
      case UnorderedList(items)  => items flatMap summaryInBlock
      case OrderedList(items, _) => items flatMap summaryInBlock
      case DefinitionList(items) => items.values.toSeq flatMap summaryInBlock
      case _                     => Nil
    }
    def summaryInInline(text: Inline): Seq[Inline] = text match {
      case Summary(text)     => List(text)
      case Chain(items)      => items flatMap summaryInInline
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
final case class Table(header: Row, columnOptions: Seq[ColumnOption], rows: Seq[Row]) extends Block
final case class ColumnOption(option: Char) { require(option == 'L' || option == 'C' || option == 'R') }
object ColumnOption {
  val ColumnOptionLeft = ColumnOption('L')
  val ColumnOptionCenter = ColumnOption('C')
  val ColumnOptionRight = ColumnOption('R')
}
final case class Row(cells: Seq[Cell])
final case class Cell(blocks: Seq[Block])

/** An section of text inside a block, possibly with formatting. */
sealed abstract class Inline

final case class Chain(items: Seq[Inline]) extends Inline
final case class Italic(text: Inline) extends Inline
final case class Bold(text: Inline) extends Inline
final case class Underline(text: Inline) extends Inline
final case class Superscript(text: Inline) extends Inline
final case class Subscript(text: Inline) extends Inline
final case class Link(target: String, title: Inline) extends Inline
final case class Monospace(text: Inline) extends Inline
final case class Text(text: String) extends Inline
abstract class EntityLink(val title: Inline) extends Inline { def link: LinkTo }
object EntityLink {
  def apply(title: Inline, linkTo: LinkTo) = new EntityLink(title) { def link: LinkTo = linkTo }
  def unapply(el: EntityLink): Some[(Inline, LinkTo)] = Some((el.title, el.link))
}
final case class HtmlTag(data: String) extends Inline {
  private val (isEnd, tagName) = data match {
    case HtmlTag.Pattern(s1, s2) =>
      (! s1.isEmpty, Some(s2.toLowerCase))
    case _ =>
      (false, None)
  }

  def canClose(open: HtmlTag) = {
    isEnd && tagName == open.tagName
  }

  def close = tagName collect {
    case name if !HtmlTag.TagsNotToClose(name) && !data.endsWith(s"</$name>") => HtmlTag(s"</$name>")
  }
}
object HtmlTag {
  private val Pattern = """(?ms)\A<(/?)(.*?)[\s>].*\z""".r
  private val TagsNotToClose = Set("br", "img")
}

/** The summary of a comment, usually its first sentence. There must be exactly one summary per body. */
final case class Summary(text: Inline) extends Inline
