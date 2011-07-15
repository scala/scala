/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml
package parsing

import java.io.File
import scala.io.Source

object ConstructingParser {
  def fromFile(inp: File, preserveWS: Boolean) =
    new ConstructingParser(Source.fromFile(inp), preserveWS) initialize

  def fromSource(inp: Source, preserveWS: Boolean) =
    new ConstructingParser(inp, preserveWS) initialize
}

/** An xml parser. parses XML and invokes callback methods of a MarkupHandler.
  * Don't forget to call next.ch on a freshly instantiated parser in order to
  * initialize it. If you get the parser from the object method, initialization
  * is already done for you.
  *
  * {{{
  * object parseFromURL {
  *   def main(args: Array[String]) {
  *     val url = args(0)
  *     val src = scala.io.Source.fromURL(url)
  *     val cpa = scala.xml.parsing.ConstructingParser.fromSource(src, false) // fromSource initializes automatically
  *     val doc = cpa.document()
  *
  *     // let's see what it is
  *     val ppr = new scala.xml.PrettyPrinter(80, 5)
  *     val ele = doc.docElem
  *     println("finished parsing")
  *     val out = ppr.format(ele)
  *     println(out)
  *   }
  * }
  * }}} */
class ConstructingParser(val input: Source, val preserveWS: Boolean)
extends  ConstructingHandler
with     ExternalSources
with     MarkupParser  {

  // default impl. of Logged
  override def log(msg: String): Unit = {}
}
