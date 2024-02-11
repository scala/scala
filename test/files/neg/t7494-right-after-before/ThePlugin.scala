package scala.test.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(val global: Global) extends Plugin {
  import global._

  val name = "rafter-before-1"
  val description = "hey it works"
  val components = List[PluginComponent](thePhase1)

  private object thePhase1 extends PluginComponent {
    val global = ThePlugin.this.global
    override def description = ThePlugin.this.description

    val runsAfter = List[String]("refchecks")
    override val runsBefore = List[String]("erasure")
    val phaseName = ThePlugin.this.name

    def newPhase(prev: Phase) = new ThePhase(prev)
  }

  private class ThePhase(prev: Phase) extends Phase(prev) {
    def name = ThePlugin.this.name
    def run: Unit = {}
  }
}

