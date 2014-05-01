/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

object Message {
  def apply[T](s: => T) = new xsbti.F0[T] { def apply() = s }
}