import scala.tools.nsc.util._

object Test {
  def main(args: Array[String]): Unit = {
    val cp = ClassPath.expandPath("""C:\FooBar\Java\includes\*.jar""") mkString java.io.File.pathSeparator
    println(cp)
  }
}
