
package t4841

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }
import scala.reflect.io.Path
import scala.reflect.io.File

/** A test plugin.  */
class Ploogin(val global: Global, val name: String = "ploogin") extends Plugin {
  import global._

  val description = "A sample plugin for testing."
  val components = List[PluginComponent](TestComponent)

  private object TestComponent extends PluginComponent {
    val global: Ploogin.this.global.type = Ploogin.this.global
    //override val runsBefore = List("refchecks")
    val runsAfter = List("jvm")
    val phaseName = Ploogin.this.name
    override def description = "A sample phase that does so many things it's kind of hard to describe briefly."
    def newPhase(prev: Phase) = new TestPhase(prev)
    class TestPhase(prev: Phase) extends StdPhase(prev) {
      override def description = TestComponent.this.description
      def apply(unit: CompilationUnit) {
        if (settings.developer) inform(s"My phase name is $phaseName")
      }
    }
  }
}
