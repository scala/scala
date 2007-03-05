/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
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
 */
class CharInputStreamIterator(in: InputStream) extends Iterator[char] {

  private var ch: int = _
  private var chSet = false
  private var error: IOException = null

  private def lookahead(): unit = try {
    ch = in.read(); chSet = ch >= 0
  } catch {
    case ex: EOFException => ch = -1
    case ex: IOException => ch = 1; error = ex
  }

  def hasNext: boolean = {
    if (!chSet) lookahead
    chSet
  }

  def next(): char = {
    if (!chSet) lookahead
    chSet = false
    ch.asInstanceOf[char]
  }
}
