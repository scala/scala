package scala.test.plugins

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ThePlugin(val global: Global) extends Plugin {
  import global._

  val name = "afterterminal"
  val description = "Declares one plugin that wants to be after the terminal phase"
  val components = List[PluginComponent](thePhase)
  
  private object thePhase extends PluginComponent {
    val global = ThePlugin.this.global

    val runsAfter = List[String]("terminal")

    val phaseName = ThePlugin.this.name

    def newPhase(prev: Phase) = new ThePhase(prev)    
  }
  
  private class ThePhase(prev: Phase) extends Phase(prev) {
    def name = ThePlugin.this.name
    def run {}
  }
}

