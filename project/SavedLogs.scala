package scala.build

import java.io.{ByteArrayOutputStream, PrintStream}

import sbt._
import Keys._
import sbt.internal.util.ConsoleAppender
import scala.collection.mutable

/** Save MiMa logs so they don't get lost in lots of debug output */
object SavedLogs {
  val savedLogs = new mutable.HashMap[String, ByteArrayOutputStream]

  val showSavedLogs = TaskKey[Unit]("showSavedLogs", "Print all saved logs to the console")
  val clearSavedLogs = TaskKey[Unit]("clearSavedLogs", "Clear all saved logs")

  def showSavedLogsImpl(println: String => Unit): Unit = synchronized {
    savedLogs.foreach {
      case (k, os) =>
        val log = new String(os.toByteArray)
        if (log.nonEmpty) {
          println(s"Saved log of $k:")
          println(log)
        }
    }
  }

  def clearSavedLogsImpl(): Unit = synchronized { savedLogs.clear() }

  lazy val settings = Seq[Setting[_]](
    (Global / extraAppenders) := {
      val previous = (Global / extraAppenders).value
      (key: ScopedKey[_]) => {
        key.scope match {
          case Scope(Select(ProjectRef(_, p)), _, Select(t), _) if t.label == "mimaReportBinaryIssues" =>
            val os = new ByteArrayOutputStream
            val a = ConsoleAppender(new PrintStream(os, true))
            SavedLogs.synchronized { savedLogs.put(s"$p/${t.label}", os) }
            a +: previous(key)
          case _ => previous(key)
        }
      }
    },

    showSavedLogs := {
      val log = streams.value.log
      showSavedLogsImpl(s => log.info(s))
    },

    clearSavedLogs := { clearSavedLogsImpl() }
  )
}
