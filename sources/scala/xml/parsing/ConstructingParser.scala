/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.parsing ;

import scala.io.Source;

object ConstructingParser {

  def fromFile(inp: java.io.File, preserveWS: Boolean) = {
    val p = new ConstructingParser(Source.fromFile(inp), preserveWS);
    p.nextch;
    p
  }

  def fromSource(inp: scala.io.Source, preserveWS: Boolean) = {
    val p = new ConstructingParser(inp, preserveWS);
    p.nextch;
    p
  }
}

/** an xml parser. parses XML and invokes callback methods of a MarkupHandler
 */
class ConstructingParser(inp: Source, presWS:Boolean)
extends  ConstructingHandler
with     ExternalSources
with     MarkupParser  {

  // default impl. of Logged
  override def log(msg:String): Unit = {}

  val preserveWS = presWS;
  val input = inp;
  val handle = this;
}

