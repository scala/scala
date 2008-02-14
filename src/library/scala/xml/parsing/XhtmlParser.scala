// $Id$

package scala.xml.parsing

import scala.io.{Source}

/**
 * Extends the Markup Parser to do the right thing (tm) with PCData blocks.
 * (c) David Pollak, 2007 WorldWide Conferencing, LLC
 */
trait PCDataMarkupParser[PCM <: MarkupParser with MarkupHandler] extends MarkupParser { self: PCM =>

  /** '&lt;! CharData ::= [CDATA[ ( {char} - {char}"]]&gt;"{char} ) ']]&gt;'
   *
   * see [15]
   */
  override def xCharData: NodeSeq = {
    xToken("[CDATA[")
    val pos1 = pos
    val sb: StringBuilder = new StringBuilder()
    while (true) {
      if (ch==']'  &&
          { sb.append(ch); nextch; ch == ']' } &&
          { sb.append(ch); nextch; ch == '>' } ) {
            sb.setLength(sb.length - 2);
            nextch;
            return PCData(sb.toString)
          } else sb.append( ch );
      nextch;
    }
    // bq: (todo) increase grace when meeting CDATA section
    throw FatalError("this cannot happen");
  }
}

/**
 * An XML Parser that preserves CDATA blocks and knows about HtmlEntities.
 * (c) David Pollak, 2007 WorldWide Conferencing, LLC
 */
class XhtmlParser(val input: Source) extends ConstructingHandler with PCDataMarkupParser[XhtmlParser] with ExternalSources  {
  val preserveWS = true
  ent ++= XhtmlEntities()
}

/**
 * Convenience method that instantiates, initializes and runs an XhtmlParser
 * (c) Burak Emir
 */
object XhtmlParser {
  def apply(source: Source): NodeSeq = {
    val p = new XhtmlParser(source)
    p.nextch
    p.document
  }
}
