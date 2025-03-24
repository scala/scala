object Test extends App {
  import scala.reflect.runtime._
  import scala.reflect.runtime.universe._
  import scala.tools.reflect.ToolBox

  val mirror = universe.runtimeMirror(universe.getClass.getClassLoader)
  val toolbox = mirror.mkToolBox()
  def showParsed(code: String) = {
    val parsed = toolbox.parse(code)
    val recovered = code.substring(parsed.pos.start, parsed.pos.end)
    println(s"\n$code\n${show(parsed, printPositions = true)}\n$recovered")
  }
  showParsed("x map f")
  showParsed("x map (f)")
  showParsed("x map ((f))")
  showParsed("x map {f}")
  showParsed("x map {{f}}")
  showParsed("x map {({(f)})}")
}
