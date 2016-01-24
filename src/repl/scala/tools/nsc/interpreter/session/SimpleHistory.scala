/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
package session

import scala.collection.mutable.{ Buffer, ListBuffer }

class SimpleHistory extends History {
  private var _index: Int = 0
  protected val buf: Buffer[String] = new ListBuffer[String]
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

  def remove(idx: Int): CharSequence        = buf remove idx
  def removeFirst(): CharSequence           = buf remove 0
  def removeLast(): CharSequence            = buf remove lastIndex
  def set(idx: Int, to: CharSequence): Unit = buf(idx) = to

  def current()         = if (index >= 0 && index < buf.size) buf(index) else fail("current()")
  def previous()        = (index > 0) && minusOne
  def next()            = (index <= lastIndex) && plusOne
  def moveToFirst()     = (size > 0) && (index != 0) && setTo(0)
  def moveToLast()      = (size > 0) && (index < lastIndex) && setTo(lastIndex)
  def moveTo(idx: Int)  = (idx > 0) && (idx <= lastIndex) && setTo(idx)
  def moveToEnd(): Unit = setTo(size)

  def asStrings = buf.toList
}
