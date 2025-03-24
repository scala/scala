object Test extends App {
  import scala.reflect.internal.util.StringContextStripMarginOps
  import scala.reflect.runtime._
  import scala.reflect.runtime.universe._
  import scala.tools.reflect.ToolBox

  val mirror = universe.runtimeMirror(universe.getClass.getClassLoader)
  val toolbox = mirror.mkToolBox()
  def showParsed(code: String) = {
    val parsed = toolbox.parse(code)
    def codeOf(pos: Position) = code.substring(pos.start, pos.end)
    val recovered = codeOf(parsed.pos)
    val pieces = parsed.collect {
      case tree @ TypeApply(fun, args) => codeOf(tree.pos)
    }
    val display =
      if (pieces.isEmpty) recovered
      else
        sm"""|$recovered
             |${pieces.mkString("\n")}"""
    println {
      sm"""|
           |$code
           |${show(parsed, printPositions = true)}
           |$display"""
    }
  }
  showParsed("List apply 1")
  showParsed("List apply[Int] 2")
  showParsed("List apply[List[Int]] (List(1), List(2)) mapConserve[List[Any]] (x => x)")
  showParsed("1 ->[Int] 2")
  //def op[A, B](i: Int): Int = 2*i
  showParsed("new A() op  [Int,   String     ]  42")
  showParsed("42 ::[Int] Nil")
}
