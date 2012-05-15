/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
package session

/** A straight scalification of the jline interface which mixes
 *  in the sparse jline-independent one too.
 */
trait JLineHistory extends JHistory with History {
  def size: Int
  def isEmpty: Boolean
  def index: Int
  def clear(): Unit
  def get(index: Int): CharSequence
  def add(line: CharSequence): Unit
  def replace(item: CharSequence): Unit

  def entries(index: Int): JListIterator[JEntry]
  def entries(): JListIterator[JEntry]
  def iterator: JIterator[JEntry]

  def current(): CharSequence
  def previous(): Boolean
  def next(): Boolean
  def moveToFirst(): Boolean
  def moveToLast(): Boolean
  def moveTo(index: Int): Boolean
  def moveToEnd(): Unit
}

object JLineHistory {
  class JLineFileHistory extends SimpleHistory with FileBackedHistory {
    override def add(item: CharSequence): Unit = {
      if (!isEmpty && last == item)
        repldbg("Ignoring duplicate entry '" + item + "'")
      else {
        super.add(item)
        addLineToFile(item)
      }
    }
    override def toString = "History(size = " + size + ", index = " + index + ")"
  }

  def apply(): JLineHistory = try new JLineFileHistory catch { case x: Exception => new SimpleHistory() }
}
