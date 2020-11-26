package scala.tools.tastytest

import scala.collection.immutable.ArraySeq
import scala.util.{Try, Success, Properties}

import javax.tools.ToolProvider

object Javac extends Script.Command {

  def javac(out: String, sources: String*): Try[Boolean] = {

    val javaCompiler = ToolProvider.getSystemJavaCompiler
    val javaCP = Properties.propOrEmpty("java.class.path")

    def compile(args: String*) =
      Try(javaCompiler.run(null, null, null, args:_*) == 0)

    if (sources.isEmpty) {
      Success(true)
    }
    else {
      val classpath = Seq(out, javaCP).filter(!_.isEmpty).mkString(Files.classpathSep)
      val settings = Array(
        "-d", out,
        "-classpath", classpath,
      ) ++ sources
      compile(ArraySeq.unsafeWrapArray(settings):_*)
    }
  }

  val commandName: String = "javac"
  val describe: String = s"$commandName <out: Directory> <src: File>"

  def process(args: String*): Int = {
    if (args.length != 2) {
      println(red(s"please provide two arguments in sub-command: $describe"))
      return 1
    }
    val Seq(out, src) = args: @unchecked
    val success = javac(out, src).get
    if (success) 0 else 1
  }

}
