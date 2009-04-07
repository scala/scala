/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Paul Phillips
 */
// $Id$

package scala.tools.nsc.interpreter

import jline._

// REPL completor - queries supplied interpreter for valid completions
// based on current contents of buffer.
class Completion(val interpreter: Interpreter) extends Completor {
  import java.util.{ List => JList }

  override def complete(_buffer: String, cursor: Int, candidates: JList[_]): Int = {
    val buffer = if (_buffer == null) "" else _buffer
    val clist = candidates.asInstanceOf[JList[String]]
    val lastDot = buffer.lastIndexOf('.')
    val (path, stub) =
      if (lastDot < 0) (buffer, "")
      else (buffer.substring(0, lastDot), buffer.substring(lastDot + 1))

    def completeMembers(needsDot: Boolean): Int = {
      def withDot(s: String) = if (needsDot) "." + s else s
      val members = interpreter membersOfIdentifier path
      members.filter(_ startsWith stub).foreach(x => clist add withDot(x))
      buffer.length - stub.length
    }

    // if there is no dot, complete on unqualified identifiers; otherwise, id's members
    lazy val ids = interpreter unqualifiedIds path
    if (lastDot >= 0) completeMembers(false)
    else ids match {
      case Nil                    => 0
      case x :: Nil if x == path  => completeMembers(true)  // only one id matches, insert the dot
      case xs                     => xs foreach (x => clist add x) ; 0
    }
  }
}
