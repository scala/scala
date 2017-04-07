import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object Test {
  def main(args: Array[String]): Unit = {
    val classloader = getClass.getClassLoader
    val toolbox = universe.runtimeMirror(classloader).mkToolBox()
    println(toolbox.compile(toolbox.parse("Array(1, 2, 3).toList")).apply())
  }
}

