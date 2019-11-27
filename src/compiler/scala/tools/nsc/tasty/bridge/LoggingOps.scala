package scala.tools.nsc.tasty.bridge

trait LoggingOps extends TastyKernel {
  final def logTasty(str: => String): Unit = {
    if (settings.debugTasty) reporter.echo(NoPosition, str)
  }
}
