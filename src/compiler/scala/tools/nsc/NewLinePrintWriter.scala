/* NSC -- new Scala compiler
 * Copyright 2005-2008 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id: Interpreter.scala 14056 2008-02-18 22:42:13Z spoon $

package scala.tools.nsc
import java.io.{Writer, PrintWriter}

class NewLinePrintWriter(out: Writer, autoFlush: Boolean)
extends PrintWriter(out, autoFlush) {
  def this(out: Writer) = this(out, false)
  override def println() { print("\n"); flush() }
}

