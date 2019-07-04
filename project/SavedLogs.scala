package scala.build

import java.io.StringWriter

import scala.collection.mutable
import sbt._
import Keys._
import org.apache.logging.log4j.core
import org.apache.logging.log4j.core.appender.{AbstractAppender, WriterAppender}
import sbt.internal.util.StringEvent

/** Save MiMa logs so they don't get lost in lots of debug output */
object SavedLogs {
  val savedLogs = new mutable.HashMap[String, mutable.ArrayBuffer[StringEvent]]

  val showSavedLogs = TaskKey[Unit]("showSavedLogs", "Print all saved logs to the console")
  val clearSavedLogs = TaskKey[Unit]("clearSavedLogs", "Clear all saved logs")

  def showSavedLogsImpl(println: String => Unit): Unit = synchronized {
    savedLogs.foreach {
      case (k, buf) if buf.nonEmpty =>
        println(s"Saved log of $k:")
        buf.foreach { e => println(e.message) }
      case _ =>
    }
  }

  def clearSavedLogsImpl(): Unit = synchronized { savedLogs.clear() }

  class MyAppender(val name: String) extends AbstractAppender(name, null, null, true) {
    start()
    val buf = new mutable.ArrayBuffer[StringEvent]
    override def append(logEvent: core.LogEvent): Unit = {
      logEvent.getMessage.getParameters match {
        case Array(s: StringEvent) => buf.append(s)
        case _ =>
      }
    }
  }

  lazy val settings = Seq[Setting[_]](
    (Global / extraLoggers) := {
      val previous = (Global / extraLoggers).value
      (key: ScopedKey[_]) => {
        key.scope match {
          case Scope(Select(ProjectRef(_, p)), _, Select(t), _) if t.label == "mimaReportBinaryIssues" =>
            val a = new MyAppender(s"$p/${t.label}")
            SavedLogs.synchronized { savedLogs.put(a.name, a.buf) }
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
