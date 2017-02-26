package scala.tools.nsc.backend.jvm

import scala.reflect.internal.Reporter
import scala.tools.nsc.Global
import scala.tools.nsc.reporters.{NoReporter, StoreReporter}
object HasReporter {
  private val localReporter = new ThreadLocal[LocalReporter]

}
trait HasReporter {
  import HasReporter.localReporter

  val global : Global

  @inline def withAstTreeLock[T](fn: => T): T = {
    global.synchronized(fn)
  }

  lazy val defaultReporter = new DefaultReporter(global)

  @inline private def reporterForThread: LocalReporter = {
    val local = localReporter.get()
    if (local eq null) defaultReporter else local
  }

  final def reporter: Reporter = reporterForThread.innerReporter

  @inline def withoutReporting[T](fn : => T) = {
    withReporterOverride(NoLocalReporter)(fn)
  }
  @inline def withReporterOverride[T](tempLocalReporter: LocalReporter)(fn : => T) = {
    val currentReporter = localReporter.get
    localReporter.set(tempLocalReporter)
    try fn finally localReporter.set(currentReporter)
  }
}

sealed trait LocalReporter {
  def innerReporter: Reporter
}

trait AsyncReporter extends LocalReporter {
  var pending: StoreReporter = _

  override def innerReporter: Reporter = {
    if (pending == null) pending = new StoreReporter
    pending
  }

  def relayReports(reporter:Reporter) = {
    val buffered = pending
    if (buffered != null) {
      buffered.infos foreach {
        case buffered.Info(pos, msg, buffered.ERROR) => reporter.error(pos, msg)
        case buffered.Info(pos, msg, buffered.INFO) => reporter.echo(pos, msg)
        case buffered.Info(pos, msg, buffered.WARNING) => reporter.warning(pos, msg)
        case buffered.Info(pos, msg, sev) => reporter.error(pos, s"UNKNOWN severity $sev -> $msg")
      }
    }
  }
}
object NoLocalReporter extends LocalReporter {
  override def innerReporter: Reporter = NoReporter
}
class DefaultReporter(val global : Global) extends LocalReporter {
  override def innerReporter: Reporter = global.reporter
}
