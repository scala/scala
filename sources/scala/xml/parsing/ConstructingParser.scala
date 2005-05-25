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
    /* DEBUG
    val src = Source.fromFile(inp);
    while(src.hasNext) {
      Console.print(src.next);
      if(!src.hasNext) {
        Console.print("last character!");
        Console.print(src.ch);
      }
    }
    */
    val p = new ConstructingParser(Source.fromFile(inp), preserveWS);
    /*
    {
      override def externalSource(systemLiteral: String): Source = {
        Source.fromFile(new java.io.File(inp.getParent(), systemLiteral));
      }
    }
    */
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
with     MarkupParser  {

  override val isValidating = true;
  val preserveWS = presWS;
  val input = inp;
  val handle = this;

  override def externalSource(systemLiteral: String): Source = {
    var fileStr = inp.descr;
    if(inp.descr.startsWith("file:")) {
      fileStr = inp.descr.substring(5, inp.descr.length());
    }
    fileStr = fileStr.substring(0,fileStr.lastIndexOf(java.io.File.separator)+1);
    Source.fromFile(fileStr + systemLiteral);
  }
}
