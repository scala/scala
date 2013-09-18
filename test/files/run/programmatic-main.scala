
import scala.language.postfixOps
import scala.tools.nsc._
import io.Path

object Test {
  val cwd = Option(System.getProperty("partest.cwd")) getOrElse "."
  val basedir = Path(cwd).parent / "lib" path
  val baseargs = Array("-usejavacp", "-bootclasspath", basedir + "/scala-library.jar", "-cp", basedir + "/scala-compiler.jar")

  def main(args: Array[String]): Unit = {
    Console.withErr(Console.out) {
      Main process (baseargs ++ "-Xpluginsdir /does/not/exist/foo/quux -Xshow-phases".split(' '))
    }
  }
}
