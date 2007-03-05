/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing

import compat.StringBuilder

/** This class ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
class SimpleTokenizer(in: Iterator[char], delimiters: String) extends Iterator[String] {

  private def max(x: int, y: char): int = if (x > y) x else y

  val tracing = false

  private def delimArray: Array[boolean] = {
    val ds = List.fromString(delimiters)
    val da = new Array[boolean]((0 /: ds)(max) + 1)
    for (val ch <- ds) { da(ch) = true }
    da
  }

  private val isdelim = delimArray
  private def isDelimiter(ch: int) = ch >= 0 && ch < isdelim.length && isdelim(ch)

  private val EOI = -1

  private def nextChar(): int = if (in.hasNext) in.next else EOI

  private var ch: int = nextChar

  private val buf = new StringBuilder()

  def hasNext: boolean = ch != EOI

  def next(): String = {
    while (ch <= ' ' && ch != EOI) ch = nextChar()
    if (ch == EOI) ""
    else {
      buf.setLength(0)
      if (isDelimiter(ch)) {
        buf append ch.asInstanceOf[char]; ch = nextChar()
      } else {
        while (ch > ' ' && ch != EOI && !isDelimiter(ch)) {
          buf append ch.asInstanceOf[char]
          ch = nextChar()
        }
      }
      if (tracing) Console.println("<" + buf.toString() + ">")
      buf.toString()
    }
  }
}

