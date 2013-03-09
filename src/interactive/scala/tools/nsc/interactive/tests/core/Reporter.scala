package scala.tools.nsc.interactive.tests.core

private[tests] trait Reporter {
  def println(msg: Any): Unit
}

/** Reporter that simply prints all messages in the standard output.*/
private[tests] object ConsoleReporter extends Reporter {
  def println(msg: Any) { Console.println(msg) }
}

/** Reporter that swallows all passed message. */
private[tests] object NullReporter extends Reporter {
  def println(msg: Any) {}
}