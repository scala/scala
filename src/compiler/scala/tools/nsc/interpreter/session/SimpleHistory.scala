/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
package session

import scala.collection.mutable.{ Buffer, ListBuffer }
import scala.collection.JavaConverters._

class SimpleHistory extends JLineHistory {
  private var _index: Int = 0
  private val buf: Buffer[String] = new ListBuffer[String]
  private def toEntries(): Seq[JEntry] = buf.zipWithIndex map { case (x, i) => Entry(i, x) }
  private def setTo(num: Int)          = { _index = num ; true }
  private def minusOne                 = { _index -= 1 ; true }
  private def plusOne                  = { _index += 1 ; true }
  private def lastIndex                = size - 1
  private def fail(msg: String): String = {
    repldbg("Internal error in history(size %d, index %d): %s".format(
      size, index, msg)
    )
    ""
  }

  case class Entry(index: Int, value: CharSequence) extends JEntry {
    override def toString = value
  }

  def maxSize: Int = 2500
  def last = if (isEmpty) fail("last") else buf.last

  def size = buf.size
  def index = _index
  def isEmpty = buf.isEmpty
  def clear() = buf.clear()
  def get(idx: Int): CharSequence = buf(idx)
  def add(item: CharSequence): Unit = buf += item
  def replace(item: CharSequence): Unit = {
    buf trimEnd 1
    add(item)
  }
  def entries(idx: Int): JListIterator[JEntry] = toEntries().asJava.listIterator(idx)
  def entries(): JListIterator[JEntry]         = toEntries().asJava.listIterator()
  def iterator: JIterator[JEntry]              = toEntries().iterator.asJava

  def current()         = if (index >= 0 && index < buf.size) buf(index) else fail("current()")
  def previous()        = (index > 0) && minusOne
  def next()            = (index <= lastIndex) && plusOne
  def moveToFirst()     = (size > 0) && (index != 0) && setTo(0)
  def moveToLast()      = (size > 0) && (index < lastIndex) && setTo(lastIndex)
  def moveTo(idx: Int)  = (idx > 0) && (idx <= lastIndex) && setTo(idx)
  def moveToEnd(): Unit = setTo(size)

  // scala legacy interface
  def asList: List[JEntry] = toEntries().toList
  def asJavaList           = entries()
  def asStrings            = buf.toList
  def grep(s: String)      = buf.toList filter (_ contains s)
}
