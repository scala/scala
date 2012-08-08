/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
import language.implicitConversions

/** Files having to do with the state of a repl session:
 *  lines of text entered, types and terms defined, etc.
 */
package object session {
  type JIterator[T]       = java.util.Iterator[T]
  type JListIterator[T]   = java.util.ListIterator[T]

  type JEntry             = scala.tools.jline.console.history.History.Entry
  type JHistory           = scala.tools.jline.console.history.History
  type JMemoryHistory     = scala.tools.jline.console.history.MemoryHistory
  type JPersistentHistory = scala.tools.jline.console.history.PersistentHistory

  private[interpreter] implicit def charSequenceFix(x: CharSequence): String = x.toString
}
