/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter.jline

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{FileSystems, Files, Path}

import _root_.jline.console.history.PersistentHistory

import scala.collection.JavaConverters._
import scala.io.Codec
import scala.reflect.internal.util.OwnerOnlyChmod
import scala.util.control.NonFatal


/** TODO: file locking.
  */
trait FileBackedHistory extends JLineHistory with PersistentHistory {
  def maxSize: Int
  import java.nio.file.StandardOpenOption.{APPEND, TRUNCATE_EXISTING}

  val charSet: Charset = implicitly[Codec].charSet

  // For a history file in the standard location, always try to restrict permission,
  // creating an empty file if none exists.
  // For a user-specified location, only lock down permissions if we're the ones
  // creating it, otherwise responsibility for permissions is up to the caller.
  private lazy val historyPath = {
    val fs = FileSystems.getDefault

    // This would really have been sufficient for our property getting infrastructure
    def prop(p: String) = Option.whenNonNull(System.getProperty(p))

    (prop("scala.shell.histfile").map(fs.getPath(_)).map{ p => if (!Files.exists(p)) secure(p); p } orElse
      prop("user.home").map(n => fs.getPath(n + s"${fs.getSeparator}${FileBackedHistory.defaultFileName}")).map(secure)
      ).getOrElse(throw new IllegalStateException("Cannot determine path for history file."))
  }

  private def secure(p: Path): Path = {
    try OwnerOnlyChmod.chmodFileOrCreateEmpty(p)
    catch { case NonFatal(e) =>
      e.printStackTrace(Console.err)
      Console.err.println(s"Warning: history file ${p}'s permissions could not be restricted to owner-only.")
    }

    p
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
      append(s"$item\n")
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
