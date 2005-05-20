/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.parsing ;

object ConstructingParser {
  def fromSource(inp: scala.io.Source, preserveWS: Boolean) = {
    val p = new ConstructingParser(inp, preserveWS);
    p.nextch;
    p
  }
}

/** an xml parser. parses XML and invokes callback methods of a MarkupHandler
 */
class ConstructingParser(inp: scala.io.Source, presWS:Boolean)
extends  ConstructingHandler
with     MarkupParser  {

  val preserveWS = presWS;
  val input = inp;
  val handle = this;

}
