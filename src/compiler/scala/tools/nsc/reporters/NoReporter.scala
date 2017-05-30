package scala.tools.nsc.reporters
import scala.reflect.internal.util.Position

/**
  * A reporter that ignores reports
  */
object NoReporter extends Reporter{
  override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = ()
}