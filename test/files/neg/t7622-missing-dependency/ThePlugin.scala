package scala.test.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(val global: Global) extends Plugin {
  import global._

  val name = "myplugin"
  val description = "Declares one plugin with a missing requirement"
  val components = List[PluginComponent](thePhase)

  private object thePhase extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]("typer")

    val phaseName = ThePlugin.this.name

    override val requires = List("missing")

    def newPhase(prev: Phase) = new ThePhase(prev)
  }

  private class ThePhase(prev: Phase) extends Phase(prev) {
    def name = thePhase.phaseName
    def run {}
  }
}

