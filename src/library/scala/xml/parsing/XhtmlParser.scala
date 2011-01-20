/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml
package parsing

import scala.io.Source

/** <p>
 *    An XML Parser that preserves CDATA blocks and knows about HtmlEntities.
 *  </p>
 *  <p>
 *    (c) David Pollak, 2007 WorldWide Conferencing, LLC.
 *  </p>
 */
class XhtmlParser(val input: Source) extends ConstructingHandler with MarkupParser with ExternalSources  {
  val preserveWS = true
  ent ++= XhtmlEntities()
}

/** <p>
 *    Convenience method that instantiates, initializes and runs an XhtmlParser.
 *  </p>
 *  <p>
 *    (c) Burak Emir
 *  </p>
 */
object XhtmlParser {
  def apply(source: Source): NodeSeq = new XhtmlParser(source).initialize.document
}
