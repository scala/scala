import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp"

  def code = """
    case class C(x: Int, y: Int)
  """.trim

  def show() {
    val global = newCompiler()
    import global._
    import analyzer._

    val output = collection.mutable.ListBuffer[String]()

    object macroPlugin extends MacroPlugin {
      override def pluginsEnterSym(namer: Namer, tree: Tree): Boolean = {
        output += s"enterSym(${tree.toString.replace('\n', ' ')})"
        namer.standardEnterSym(tree)
        true
      }
      override def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Option[Symbol] = {
        output += s"ensureCompanionObject(${cdef.toString.replace('\n', ' ')}, ...)"
        Some(namer.standardEnsureCompanionObject(cdef, creator))
      }
      override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
        stats.foreach(stat => output += s"enterStat(${stat.toString.replace('\n', ' ')})")
        stats
      }
    }

    addMacroPlugin(macroPlugin)
    compileString(global)(code)
    println(output.mkString("\n"))
  }
}
