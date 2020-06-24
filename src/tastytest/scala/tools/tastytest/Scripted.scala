package scala.tools.tastytest

object Scripted {

  private val commands = List(Dotc.describe, DotcDecompiler.describe, Scalac.describe, Runner.describe)

  def process(args: String*): Int = {
    if (args.isEmpty) {
      println(red("Please provide at least one sub-command"))
      return 1
    }
    val Seq(command, args0 @ _*) = args
    command match {
      case Dotc.commandName           => Dotc.process(args0:_*)
      case DotcDecompiler.commandName => DotcDecompiler.process(args0:_*)
      case Scalac.commandName         => Scalac.process(args0:_*)
      case Runner.commandName         => Runner.process(args0:_*)
      case other      =>
        println(red(s"unrecognised sub-command $other, try one of ${commands.mkString("\n  ","\n  ","")}"))
        1
    }
  }

  def main(args: Array[String]): Unit = sys.exit(process(args.toIndexedSeq: _*))

}
