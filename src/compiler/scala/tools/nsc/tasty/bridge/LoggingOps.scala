package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse

trait LoggingOps { self: TastyUniverse =>
  final def logTasty(str: => String): Unit = {
    if (settings.YdebugTasty) reporter.echo(noPosition, str)
  }
}
