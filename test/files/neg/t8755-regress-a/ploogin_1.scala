
package t8755

import scala.tools.nsc, nsc.{Global, Phase, plugins}, plugins.{Plugin, PluginComponent}

class P(val global: Global) extends Plugin {
  override val name = "Testing phase assembly"
  override val description = "C7 is not dropped even though it has no runs[Right]After"
  override val components = List[PluginComponent](
    component7,
    component8,
  )

  object component7 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C7"
    override val description = "C7 has only a before constraint"
    override val runsRightAfter = None
    override val runsAfter = Nil
    override val runsBefore = List("patmat")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component8 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C8"
    override val description = "C8 makes C7 reachable"
    override val runsRightAfter = None
    override val runsAfter = List("typer")
    override val runsBefore = List("C7") // component name, not phase name!
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
}
