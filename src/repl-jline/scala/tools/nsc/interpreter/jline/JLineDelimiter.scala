/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter.jline

import scala.tools.nsc.interpreter

import _root_.jline.console.completer.ArgumentCompleter.{ ArgumentDelimiter, ArgumentList }

// implements a jline interface
class JLineDelimiter extends ArgumentDelimiter {
  def toJLine(args: List[String], cursor: Int): ArgumentList = args match {
    case Nil => new ArgumentList(new Array[String](0), 0, 0, cursor)
    case xs => new ArgumentList(xs.toArray, xs.size - 1, xs.last.length, cursor)
  }

  def delimit(buffer: CharSequence, cursor: Int) = {
    val p = interpreter.Parsed(buffer.toString, cursor)
    toJLine(p.args, cursor)
  }

  def isDelimiter(buffer: CharSequence, cursor: Int) = interpreter.Parsed(buffer.toString, cursor).isDelimiter
}
