package scala.tools.tastytest

object Scripted extends Script {

  val subcommands = List(Dotc, DotcDecompiler, PrintTasty, Scalac, Runner, Javac)
  val commandName = "Scripted"

}
