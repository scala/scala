package scala.tools.nsc.transform

import org.junit.jupiter.api.Test

import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class ReleaseFenceTest extends BytecodeTesting {
  import compiler._

  @Test
  def finalValInParentTraitCausesReleaseFenceInSubclassConstructor(): Unit = {
    val code =
      """class C extends T { println(""); object Inner extends T { println("") }}; object O extends T { println("") }; trait T { final val x: String = "" } """
    val classes = compileClasses(code)
    def check(className: String, hasFence: Boolean): Unit = {
      val cnode = classes.find(_.name == className).get
      if (hasFence)
        assertInvoke(getMethod(cnode, "<init>"), "scala/runtime/Statics", "releaseFence")
      else
        assertDoesNotInvoke(getMethod(cnode, "<init>"), "releaseFence")
    }
    check("C", hasFence = true)
    check("C$Inner$", hasFence = true)
    // top level module initialization is guarded within the <clinit> lock, we don't need to emit releaseFence
    check("O$", hasFence = false)
  }
}
