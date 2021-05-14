package scala.tools.tastytest

import scala.util.{Try, Success, Failure}

object DotcDecompiler extends Script.Command {

  private def dotcProcess(args: Seq[String])(implicit cl: Dotc.ClassLoader) =
    Dotc.processMethod("dotty.tools.dotc.decompiler.Main")(args)

  def decompile(source: String, additionalSettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Boolean] =
    dotcProcess(("-usejavacp" +: additionalSettings :+ source))

  val commandName: String = "dotcd"
  val describe: String = s"$commandName <tasty: File> <args: String*>"

  def process(args: String*): Int = {
    if (args.length < 1) {
      println(red(s"please provide at least 1 argument in sub-command: $describe"))
      return 1
    }
    val Seq(tasty, additionalSettings @ _*) = args: @unchecked
    implicit val scala3classloader: Dotc.ClassLoader = Dotc.initClassloader() match {
      case Success(cl) => cl
      case Failure(err) =>
        println(red(s"could not initialise Scala 3 classpath: $err"))
        return 1
    }
    val success = decompile(tasty, additionalSettings).get
    if (success) 0 else 1
  }

}
