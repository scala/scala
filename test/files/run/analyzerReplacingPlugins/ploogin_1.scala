package analyzerReplacingPlugins

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ EarlyPlugin, PluginComponent }
import scala.tools.nsc.typechecker.Analyzer
import scala.reflect.io.Path
import scala.reflect.io.File
import scala.reflect.internal.Mode

/** A test plugin.  */
class Ploogin(val global: Global) extends EarlyPlugin {
  import global._

  val name = "analyzerReplacingPlugin"
  val description = "A sample analyzer replacing plugin for testing."
  val components = Nil
  override val analyzer: Option[Analyzer] = Some(new { val global: Ploogin.this.global.type = Ploogin.this.global } with MyAnalyzer)

  private trait MyAnalyzer extends Analyzer {
    val global: Ploogin.this.global.type
    override def newTyper(context: Context) = new Typer(context) {
      override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
        val tree1 = tree match {
          case tree @ Literal(Constant("hello")) if tree.tpe == null => Literal(Constant("hello world"))
          case _ => tree
        }
        super.typed1(tree1, mode, pt)
      }
    }
  }
}
