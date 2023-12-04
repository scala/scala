import java.io.File.pathSeparator
import scala.tools.nsc.util.ClassPath

object Test {
  def main(args: Array[String]): Unit = println {
    ClassPath.expandPath("""C:\FooBar\Java\includes\*.jar""", expandStar = false).mkString(pathSeparator)
  }
}
