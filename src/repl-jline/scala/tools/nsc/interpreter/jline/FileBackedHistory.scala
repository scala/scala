/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter.jline

import _root_.jline.console.history.PersistentHistory

import scala.tools.nsc.interpreter
import scala.reflect.io.{File, Path}
import scala.tools.nsc.Properties.{propOrNone, userHome}
import scala.reflect.internal.util.OwnerOnlyChmod
import scala.util.control.NonFatal

/** TODO: file locking.
  */
trait FileBackedHistory extends JLineHistory with PersistentHistory {
  def maxSize: Int

  // For a history file in the standard location, always try to restrict permission,
  // creating an empty file if none exists.
  // For a user-specified location, only lock down permissions if we're the ones
  // creating it, otherwise responsibility for permissions is up to the caller.
  protected lazy val historyFile: File = File {
    propOrNone("scala.shell.histfile").map(Path.apply) match {
      case Some(p) => if (!p.exists) secure(p) else p
      case None => secure(Path(userHome) / FileBackedHistory.defaultFileName)
    }
  }

  private def secure(p: Path): Path = {
    try OwnerOnlyChmod.chmodFileOrCreateEmpty(p.jfile.toPath)
    catch { case NonFatal(e) =>
      if (interpreter.isReplDebug) e.printStackTrace()
      interpreter.replinfo(s"Warning: history file ${p}'s permissions could not be restricted to owner-only.")
    }

    p
  }

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

    val lines: IndexedSeq[String] = {
      try historyFile.lines().toIndexedSeq
      catch {
        // It seems that control characters in the history file combined
        // with the default codec can lead to nio spewing exceptions.  Rather
        // than abandon hope we'll try to read it as ISO-8859-1
        case _: Exception =>
          try historyFile.lines("ISO-8859-1").toIndexedSeq
          catch {
            case _: Exception => Vector()
          }
      }
    }

    interpreter.repldbg("Loading " + lines.size + " into history.")

    // avoid writing to the history file
    withoutSaving(lines takeRight maxSize foreach add)
    // truncate the history file if it's too big.
    if (lines.size > maxSize) {
      interpreter.repldbg("File exceeds maximum size: truncating to " + maxSize + " entries.")
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

  final val defaultFileName = ".scala_history"
}
