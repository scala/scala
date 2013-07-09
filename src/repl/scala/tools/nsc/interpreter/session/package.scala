/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
import scala.language.implicitConversions

/** Files having to do with the state of a repl session:
 *  lines of text entered, types and terms defined, etc.
 */
package object session {
  type JIterator[T]       = java.util.Iterator[T]
  type JListIterator[T]   = java.util.ListIterator[T]

  type JEntry             = jline.console.history.History.Entry
  type JHistory           = jline.console.history.History
  type JMemoryHistory     = jline.console.history.MemoryHistory
  type JPersistentHistory = jline.console.history.PersistentHistory

  private[interpreter] implicit def charSequenceFix(x: CharSequence): String = x.toString
}
