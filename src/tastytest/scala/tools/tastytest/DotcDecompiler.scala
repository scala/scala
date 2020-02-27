package scala.tools.tastytest

import scala.util.{ Try, Success }

import java.lang.reflect.Modifier

object DotcDecompiler {

  private[this] lazy val dotcProcess = Dotc.processMethod("dotty.tools.dotc.decompiler.Main")

  def decompile(source: String, additionalSettings: Seq[String]): Try[Boolean] =
    dotcProcess(("-usejavacp" +: additionalSettings :+ source).toArray)

  def main(args: Array[String]): Unit = {
    val Array(src, additionalSettings @ _*) = args
    val success = decompile(src, additionalSettings).get
    sys.exit(if (success) 0 else 1)
  }
}
