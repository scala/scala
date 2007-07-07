/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
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
 *  @deprecated use classes from scala.util.parsing.input instead
 */
class CharInputStreamIterator(in: InputStream) extends Iterator[Char] {

  private var ch: Int = _
  private var chSet = false
  private var error: IOException = null

  private def lookahead(): Unit = try {
    ch = in.read(); chSet = ch >= 0
  } catch {
    case ex: EOFException => ch = -1
    case ex: IOException => ch = 1; error = ex
  }

  def hasNext: Boolean = {
    if (!chSet) lookahead
    chSet
  }

  def next(): Char = {
    if (!chSet) lookahead
    chSet = false
    ch.asInstanceOf[Char]
  }
}
