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

  override def complete(buffer: String, cursor: Int, candidates: JList[_]): Int = {
    if (buffer == null) return 0
    val clist = candidates.asInstanceOf[JList[String]]
    val lastDot = buffer.lastIndexOf('.')
    val (path, stub) =
      if (lastDot < 0) (buffer, "")
      else (buffer.substring(0, lastDot), buffer.substring(lastDot + 1))

    val members = interpreter membersOfIdentifier path
    members.filter(_ startsWith stub).foreach(x => clist add x)
    buffer.length - stub.length
  }
}
