/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stephane Micheloud
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

object ManPage {
  abstract class AbstractText {
    def &(more: AbstractText) = SeqText(this, more)
  }

  case class SeqText(components: AbstractText*) extends AbstractText
  case class SeqPara(components: AbstractText*) extends AbstractText
  case class Text(text: String) extends AbstractText
  case object BSlash extends AbstractText
  case object MDash extends AbstractText
  case object NDash extends AbstractText
  case class Bold(contents: AbstractText) extends AbstractText
  case class Italic(contents: AbstractText) extends AbstractText
  case class Emph(contents: AbstractText) extends AbstractText
  case class Mono(contents: AbstractText) extends AbstractText
  case class Quote(contents: AbstractText) extends AbstractText
  implicit def str2text(str: String) = Text(str)

  case class Definition(term: AbstractText, description: AbstractText)
  case class DefinitionList(definitions: Definition*) extends AbstractText
  case class Link(label: AbstractText, url: String) extends AbstractText

  case class DefnItem(header: String, text: AbstractText)

  abstract class Paragraph
  case class TextParagraph(text: AbstractText) extends Paragraph
  case class CodeSample(text: String) extends Paragraph
  case class BlockQuote(text: AbstractText) extends Paragraph
  implicit def text2para(text: AbstractText): Paragraph = TextParagraph(text)
  implicit def str2para(str: String) = text2para(str2text(str))

  case class BulletList(items: AbstractText*) extends Paragraph
  case class NumberedList(items: AbstractText*) extends Paragraph
  case class TitledPara(title: String, text: AbstractText) extends Paragraph

  case class EmbeddedSection(section: Section) extends Paragraph
  implicit def section2Para(section: Section) = EmbeddedSection(section)

  case class Section(title: String, paragraphs: Paragraph*)

  object Category extends Enumeration {
    val USER_COMMANDS = Value(1, "USER COMMANDS")
    val SYSTEM_CALLS  = Value(2, "SYSTEM CALLS")
    val SUBROUTINES   = Value(3, "SUBROUTINES")
    val DEVICES       = Value(4, "DEVICES")
    val FILE_FORMATS  = Value(5, "FILE FORMAT DESCRIPTIONS")
    val GAMES         = Value(6, "GAMES")
    val MISCELLANEOUS = Value(7, "MISCELLANEOUS")
  }

  abstract class Document {
    import Category._
    var title: String = ""
    var author: String = ""
    var date: String = ""
    var version: String = ""
    var category: Value = USER_COMMANDS
    var encoding: String = "iso-8859-1"
    var sections: List[Section] = Nil
  }
}
