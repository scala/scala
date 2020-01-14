package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse

trait LoggingOps extends TastyKernel { self: TastyUniverse =>
  final def logTasty(str: => String): Unit = {
    if (settings.debugTasty) reporter.echo(noPosition, str)
  }
}
