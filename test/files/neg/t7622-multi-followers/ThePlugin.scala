package scala.test.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(val global: Global) extends Plugin {
  import global._

  val name = "multi"
  val description = "Declares two phases that both follow parser"
  val components = List[PluginComponent](thePhase1,thePhase2)

  private object thePhase1 extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]()

    override val runsRightAfter = Some("parser")

    val phaseName = ThePlugin.this.name + "1"

    def newPhase(prev: Phase) = new ThePhase(prev, phaseName)
  }

  private object thePhase2 extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]()

    override val runsRightAfter = Some("parser")

    val phaseName = ThePlugin.this.name + "2"

    def newPhase(prev: Phase) = new ThePhase(prev, phaseName)
  }

  private class ThePhase(prev: Phase, val name: String) extends Phase(prev) {
    def run {}
  }
}

