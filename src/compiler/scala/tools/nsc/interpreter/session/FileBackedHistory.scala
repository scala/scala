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
  protected def useShutdownHook = true

  // A tempfile holding only the last few lines of input
  private val bufferFile = File.makeTemp("scala_history")
  private var bufferWriter = bufferFile.printWriter()

  // flush to the permanent log every `flushFrequency` lines
  protected def flushFrequency: Int = if (useShutdownHook) 15 else 5
  private var flushCounter: Int = flushFrequency

  private def autoflush(): Unit = {
    flushCounter -= 1
    if (flushCounter <= 0) {
      flush()
      flushCounter = flushFrequency
    }
  }

  locally {
    load()
    if (useShutdownHook)
      sys addShutdownHook flush()
  }
  private def drainBufferFile() = {
    if (bufferWriter != null)
      bufferWriter.close()

    try     bufferFile.lines().toList map (_ + "\n")
    finally bufferWriter = bufferFile.printWriter()
  }

  private var isPersistent = true
  def withoutSaving[T](op: => T): T = {
    val saved = isPersistent
    isPersistent = false
    try op
    finally isPersistent = saved
  }
  def addLineToFile(item: CharSequence): Unit = {
    if (isPersistent) {
      bufferWriter println item
      autoflush()
    }
  }

  protected def sync(): Unit = {
    val lines = asStrings map (_ + "\n")
    historyFile.writeAll(lines: _*)
  }

  def load() = {
    val lines = historyFile.lines().toIndexedSeq
    repldbg("Loading " + lines.size + " into history.")

    // bypass the tempfile buffer
    withoutSaving(lines takeRight maxSize foreach add)
    // truncate the main history file if it's too big.
    if (lines.size > maxSize) {
      repldbg("File exceeds maximum size: truncating to " + maxSize + " entries.")
      sync()
    }
  }
  def flush(): Unit = {
    val toAppend = drainBufferFile()
    repldbg("Moving " + toAppend.size + " lines from buffer to permanent history.")
    historyFile.appendAll(toAppend: _*)
  }
  def purge(): Unit = historyFile.truncate()
}

object FileBackedHistory {
  //   val ContinuationChar = '\003'
  //   val ContinuationNL: String = Array('\003', '\n').mkString
  import Properties.userHome

  def defaultFileName = ".scala_history"
  def defaultFile: File = File(Path(userHome) / defaultFileName)
}
