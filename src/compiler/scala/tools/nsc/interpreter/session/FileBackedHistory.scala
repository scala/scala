/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
package session

import scala.tools.nsc.io._
import FileBackedHistory._

/** TODO: file locking.
 */
trait FileBackedHistory extends JLineHistory with JPersistentHistory {
  def maxSize: Int
  protected lazy val historyFile: File = defaultFile
  private var isPersistent = true

  locally {
    load()
  }

  def withoutSaving[T](op: => T): T = {
    val saved = isPersistent
    isPersistent = false
    try op
    finally isPersistent = saved
  }
  def addLineToFile(item: CharSequence): Unit = {
    if (isPersistent)
      append(item + "\n")
  }

  /** Overwrites the history file with the current memory. */
  protected def sync(): Unit = {
    val lines = asStrings map (_ + "\n")
    historyFile.writeAll(lines: _*)
  }
  /** Append one or more lines to the history file. */
  protected def append(lines: String*): Unit = {
    historyFile.appendAll(lines: _*)
  }

  def load(): Unit = {
    if (!historyFile.canRead)
      historyFile.createFile()

    val lines: IndexedSeq[String] =
      try historyFile.lines().toIndexedSeq
      catch { case _: Exception => Vector() }

    repldbg("Loading " + lines.size + " into history.")

    // avoid writing to the history file
    withoutSaving(lines takeRight maxSize foreach add)
    // truncate the history file if it's too big.
    if (lines.size > maxSize) {
      repldbg("File exceeds maximum size: truncating to " + maxSize + " entries.")
      sync()
    }
    moveToEnd()
  }
  def flush(): Unit = ()
  def purge(): Unit = historyFile.truncate()
}

object FileBackedHistory {
  //   val ContinuationChar = '\003'
  //   val ContinuationNL: String = Array('\003', '\n').mkString
  import Properties.userHome

  def defaultFileName = ".scala_history"
  def defaultFile: File = File(Path(userHome) / defaultFileName)
}
