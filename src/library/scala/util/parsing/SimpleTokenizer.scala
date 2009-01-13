/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing

/** This class ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 *
 *  @deprecated use <a target="contentFrame" href="combinator/lexical/StdLexical.html">
 *              <code>scala.util.parsing.combinator.lexical.StdLexical</code></a>
 *              instead.
 */
@deprecated
class SimpleTokenizer(in: Iterator[Char], delimiters: String) extends Iterator[String] {

  private def max(x: Int, y: Char): Int = if (x > y) x else y

  val tracing = false

  private def delimArray: Array[Boolean] = {
    val ds = List.fromString(delimiters)
    val da = new Array[Boolean]((0 /: ds)(max) + 1)
    for (ch <- ds) da(ch) = true
    da
  }

  private val isdelim = delimArray
  private def isDelimiter(ch: Int) = ch >= 0 && ch < isdelim.length && isdelim(ch)

  private val EOI = -1

  private def nextChar(): Int = if (in.hasNext) in.next else EOI

  private var ch: Int = nextChar

  private val buf = new StringBuilder()

  def hasNext: Boolean = ch != EOI

  def next(): String = {
    while (ch <= ' ' && ch != EOI) ch = nextChar()
    if (ch == EOI) ""
    else {
      buf setLength 0
      if (isDelimiter(ch)) {
        buf append ch.asInstanceOf[Char]; ch = nextChar()
      } else {
        while (ch > ' ' && ch != EOI && !isDelimiter(ch)) {
          buf append ch.asInstanceOf[Char]
          ch = nextChar()
        }
      }
      if (tracing) Console.println("<" + buf.toString() + ">")
      buf.toString()
    }
  }
}

