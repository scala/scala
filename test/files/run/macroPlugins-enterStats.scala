import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Xprint:typer"

  def code = """
    class C {
      def x = 2
      def y = 3
    }
  """.trim

  def show() {
    val global = newCompiler()
    import global._
    import analyzer._

    val output = collection.mutable.ListBuffer[String]()
    def log(what: String) = output += what.replace(String.format("%n"), " ")

    def logEnterStat(pluginName: String, stat: Tree): Unit = log(s"$pluginName:enterStat($stat)")
    def deriveStat(pluginName: String, typer: Typer, stat: Tree): List[Tree] = stat match {
      case DefDef(mods, name, Nil, Nil, TypeTree(), body) =>
        val derived = DefDef(NoMods, TermName(name + pluginName), Nil, Nil, TypeTree(), Ident(TermName("$qmark$qmark$qmark")))
        newNamer(typer.context).enterSym(derived)
        List(derived)
      case _ =>
        Nil
    }

    object macroPlugin1 extends MacroPlugin {
      override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
        stats.foreach(stat => logEnterStat("macroPlugin1", stat))
        stats.flatMap(stat => stat +: deriveStat("macroPlugin1", typer, stat))
      }
    }
    object macroPlugin2 extends MacroPlugin {
      override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
        stats.foreach(stat => logEnterStat("macroPlugin2", stat))
        stats.flatMap(stat => stat +: deriveStat("macroPlugin2", typer, stat))
      }
    }

    addMacroPlugin(macroPlugin1)
    addMacroPlugin(macroPlugin2)
    compileString(global)(code)
    println(output.mkString("\n"))
  }
}
