/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter.jline

import java.util.{Iterator => JIterator, ListIterator => JListIterator}

import _root_.jline.{console => jconsole}
import jconsole.history.History.{Entry => JEntry}
import jconsole.history.{History => JHistory}

import scala.tools.nsc.interpreter
import scala.tools.nsc.interpreter.session.{History, SimpleHistory}


/** A straight scalafication of the jline interface which mixes
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

  override def historicize(text: String): Boolean = {
    text.lines foreach add
    moveToEnd()
    true
  }
}

object JLineHistory {
  class JLineFileHistory extends SimpleHistory with FileBackedHistory {
    override def add(item: CharSequence): Unit = {
      if (!isEmpty && last == item)
        interpreter.repldbg("Ignoring duplicate entry '" + item + "'")
      else {
        super.add(item)
        addLineToFile(item)
      }
    }
    override def toString = "History(size = " + size + ", index = " + index + ")"

    import scala.collection.JavaConverters._

    override def asStrings(from: Int, to: Int): List[String] =
      entries(from).asScala.take(to - from).map(_.value.toString).toList

    case class Entry(index: Int, value: CharSequence) extends JEntry {
      override def toString = value.toString
    }

    private def toEntries(): Seq[JEntry] = buf.zipWithIndex map { case (x, i) => Entry(i, x)}
    def entries(idx: Int): JListIterator[JEntry] = toEntries().asJava.listIterator(idx)
    def entries(): JListIterator[JEntry] = toEntries().asJava.listIterator()
    def iterator: JIterator[JEntry] = toEntries().iterator.asJava
  }

  def apply(): History = try new JLineFileHistory catch { case x: Exception => new SimpleHistory() }
}
