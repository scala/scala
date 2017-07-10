/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter.jline

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{FileSystems, Files, Path}
import java.util

import _root_.jline.console.history.PersistentHistory

import scala.collection.JavaConverters._
import scala.io.Codec


/** TODO: file locking.
  */
trait FileBackedHistory extends JLineHistory with PersistentHistory {
  def maxSize: Int
  import java.nio.file.StandardOpenOption.{CREATE, APPEND, TRUNCATE_EXISTING}

  val charSet: Charset = implicitly[Codec].charSet

  private lazy val historyPath = {
    val fs = FileSystems.getDefault

    // This would really have been sufficient for our property getting infrastructure
    def prop(p: String) = Option(System.getProperty(p))

    (prop("scala.shell.histfile") orElse
      prop("user.home").map(_ + s"${fs.getSeparator}${FileBackedHistory.defaultFileName}")
      ).map(n => fs.getPath(n)).getOrElse(throw new IllegalStateException("Cannot determine path for history file."))
  }

  protected lazy val lines: List[String] = {
    try Files.readAllLines(historyPath, charSet).asScala.toList
    catch {
      // It seems that control characters in the history file combined
      // with the default codec can lead to nio spewing exceptions.  Rather
      // than abandon hope we'll try to read it as ISO-8859-1
      case _: IOException =>
        try Files.readAllLines(historyPath, Codec.ISO8859.charSet).asScala.toList
        catch {
          case _: IOException => Nil
        }
    }
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
  protected def sync(): Unit =
    Files.write(historyPath, asStrings.asJava, charSet, TRUNCATE_EXISTING)

  /** Append one or more lines to the history file. */
  protected def append(newLines: String*): Unit =
    Files.write(historyPath, newLines.asJava, charSet, APPEND)

  def load(): Unit = try {
    // avoid writing to the history file
    withoutSaving(lines takeRight maxSize foreach add)

    // truncate the history file if it's too big.
    if (lines.size > maxSize) {
      sync()
    }

    moveToEnd()
  } catch {
    case _: IOException | _: IllegalStateException =>
      Console.err.println("Could not load history.")
      isPersistent = false
  }

  def flush(): Unit = ()

  def purge(): Unit = Files.write(historyPath, Array.emptyByteArray)
}

object FileBackedHistory {
  //   val ContinuationChar = '\003'
  //   val ContinuationNL: String = Array('\003', '\n').mkString

  final val defaultFileName = ".scala_history"
}
