/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing


import java.io.InputStream
import java.io.{IOException, EOFException}

/** This class ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 *
 *  @deprecated use classes from <a target="contentFrame" href="input.html">
 *              <code>scala.util.parsing.input</code></a> instead.
 */
@deprecated
class CharInputStreamIterator(in: InputStream) extends Iterator[Char] {

  private var ch: Int = _
  private var chSet = false
  private var error: IOException = null

  private def lookahead() {
    try {
      ch = in.read(); chSet = ch >= 0
    } catch {
      case ex: EOFException => ch = -1
      case ex: IOException => ch = 1; error = ex
    }
  }

  def hasNext: Boolean = {
    if (!chSet) lookahead()
    chSet
  }

  def next(): Char = {
    if (!chSet) lookahead()
    chSet = false
    ch.asInstanceOf[Char]
  }
}
