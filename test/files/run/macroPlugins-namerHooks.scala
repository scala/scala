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
    def log(what: String) = output += what.replace(String.format("%n"), " ")

    object macroPlugin extends MacroPlugin {
      override def pluginsEnterSym(namer: Namer, tree: Tree): Boolean = {
        log(s"enterSym($tree)")
        namer.standardEnterSym(tree)
        true
      }
      override def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Option[Symbol] = {
        log(s"ensureCompanionObject($cdef, ...)")
        Some(namer.standardEnsureCompanionObject(cdef, creator))
      }
      override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
        stats.foreach(stat => log(s"enterStat($stat)"))
        stats
      }
    }

    addMacroPlugin(macroPlugin)
    compileString(global)(code)
    println(output.mkString("\n"))
  }
}
