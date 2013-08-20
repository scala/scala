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
  override val analyzer: Option[Analyzer] = Some(global.analyzer)
}
