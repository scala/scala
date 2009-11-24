/* NSC -- new Scala compiler -- Copyright 2007-2009 LAMP/EPFL */

package scala.tools.nsc
package doc
package model
package comment

import scala.collection._

import java.net.URL

/** */
final case class Body(blocks: Seq[Block])

/** */
sealed abstract class Block

final case class Title(text: Inline, level: Int) extends Block
final case class Paragraph(text: Inline) extends Block
final case class Code(data: String) extends Block
final case class UnorderedList(items: Seq[Block]) extends Block
final case class OrderedList(items: Seq[Block]) extends Block
final case class DefinitionList(items: SortedMap[Inline, Block]) extends Block
final case class HorizontalRule() extends Block

/** */
sealed abstract class Inline

final case class Chain(items: Seq[Inline]) extends Inline
final case class Italic(text: Inline) extends Inline
final case class Bold(text: Inline) extends Inline
final case class Underline(text: Inline) extends Inline
final case class Superscript(text: Inline) extends Inline
final case class Subscript(text: Inline) extends Inline
final case class Link(raw: String) extends Inline // TODO
final case class Monospace(text: String) extends Inline
final case class Text(text: String) extends Inline
