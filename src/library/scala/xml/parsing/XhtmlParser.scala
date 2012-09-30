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

/** An XML Parser that preserves `CDATA` blocks and knows about
 *  [[scala.xml.parsing.XhtmlEntities]].
 *
 *  @author (c) David Pollak, 2007 WorldWide Conferencing, LLC.
 */
class XhtmlParser(val input: Source) extends ConstructingHandler with MarkupParser with ExternalSources  {
  val preserveWS = true
  ent ++= XhtmlEntities()
}

/** Convenience method that instantiates, initializes and runs an `XhtmlParser`.
 *
 *  @author Burak Emir
 */
object XhtmlParser {
  def apply(source: Source): NodeSeq = new XhtmlParser(source).initialize.document
}
