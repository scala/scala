package scala.test.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(val global: Global) extends Plugin {
  import global._

  val name = "timebomb"
  val description = "Explodes if run. Maybe I haven't implemented it yet."
  val components = List[PluginComponent](thePhase1)

  private object thePhase1 extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]("parser")
    override val runsBefore = List[String]("namer")
    val phaseName = ThePlugin.this.name

    def newPhase(prev: Phase) = new ThePhase(prev)
  }

  private class ThePhase(prev: Phase) extends Phase(prev) {
    override def name = ThePlugin.this.name
    override def run  = ???
  }
}

