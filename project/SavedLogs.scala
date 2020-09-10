package scala.build

// The Log4J-based API used here is deprecated in sbt 1.4, as per sbt/sbt#5731.  For now, we
// continue to use the API, and we sneakily suppress the deprecation warnings.  Moving to the new
// API would be a welcome contribution, especially given that the old API was deprecated because it
// was memory-leak-prone.  (Whether our use of the API is resulting in memory leaks, I don't know.)

import java.io.StringWriter

import scala.collection.mutable
import sbt._
import Keys._
import org.apache.logging.log4j.core
import org.apache.logging.log4j.core.appender.{AbstractAppender, WriterAppender}
import sbt.internal.util.StringEvent

/** Save MiMa logs so they don't get lost in lots of debug output */

object SavedLogs {

  // rigmarole to access deprecated key without incurring a deprecation warning
  val deprecatedExtraLoggersKey = {
    @deprecated("", "") class Sneaky { def key = extraLoggers }; object Sneaky extends Sneaky
    Sneaky.key
  }

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

  class MyAppender(val name: String) extends AbstractAppender(name, null, null, true, Array()) {
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
    (Global / deprecatedExtraLoggersKey) := {
      val previous = (Global / deprecatedExtraLoggersKey).value
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
