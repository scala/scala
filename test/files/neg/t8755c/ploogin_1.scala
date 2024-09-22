
package t8755

import scala.tools.nsc
import nsc.{Global, Phase, plugins}
import plugins.{Plugin, PluginComponent}

class P(val global: Global) extends Plugin {
  override val name = "Testing"
  override val description = "Testing phase assembly"
  override val components = List[PluginComponent](
    component1,
    //component2,
    //component3,
    //component4,
    //component5,
    component6,
    //component7,
    //component8,
  )

  object component1 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C1"
    override val description = "C1 tests phase assembly"
    override val runsRightAfter = Option("typer")
    override val runsAfter = List("typer")
    override val runsBefore = List("terminal")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component2 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C2"
    override val description = "C2 tests phase assembly impossible constraint"
    override val runsRightAfter = Option("patmat")
    override val runsAfter = List("typer")
    override val runsBefore = List("typer")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component3 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C3"
    override val description = "C3 tests phase assembly missing before, phase is ignored"
    override val runsRightAfter = None
    override val runsAfter = List("typer")
    override val runsBefore = List("germinal")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component4 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C4"
    override val description = "C4 tests phase assembly impossible constraint not right after"
    override val runsRightAfter = None
    override val runsAfter = List("typer")
    override val runsBefore = List("typer")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component5 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C5"
    override val description = "C5 tests phase assembly before a phase missing in Scaladoc"
    override val runsRightAfter = None
    override val runsAfter = List("typer")
    override val runsBefore = List("erasure")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component6 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C6"
    override val description = "C6 tests phase assembly after a phase missing in Scaladoc"
    override val runsRightAfter = None
    override val runsAfter = List("patmat")
    //override val runsBefore = List("terminal")
    override def newPhase(prev: Phase) = new phase(prev)
    class phase(prev: Phase) extends Phase(prev) {
      override val name = s"phase $phaseName"
      override def run() = println(name)
    }
  }
  object component7 extends PluginComponent {
    override val global = P.this.global
    override val phaseName = "C7"
    override val description = "C7 tests phase assembly if only a before constraint"
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
    override val description = "C8 is before C7 which specifies no after"
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
