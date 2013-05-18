package scala.test.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(val global: Global) extends Plugin {
  import global._

  val name = "cyclicdependency"
  val description = "Declares two phases that have a cyclic dependency"
  val components = List[PluginComponent](thePhase1,thePhase2)
  
  private object thePhase1 extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]("tailcalls","cyclicdependency2")

    val phaseName = ThePlugin.this.name + "1"

    def newPhase(prev: Phase) = new ThePhase(prev)    
  }
  
  private object thePhase2 extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]("dce","cyclicdependency1")

    val phaseName = ThePlugin.this.name + "2"

    def newPhase(prev: Phase) = new ThePhase(prev)    
  }
  
  private class ThePhase(prev: Phase) extends Phase(prev) {
    def name = ThePlugin.this.name
    def run {}
  }
}

