package scala.tools.tastytest

object Scripted extends Script {

  val subcommands = List(Dotc, DotcDecompiler, Scalac, Runner, Javac)
  val commandName = "Scripted"

}
