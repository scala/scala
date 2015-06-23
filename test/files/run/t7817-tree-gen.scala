import scala.tools.partest._

// Testing that `mkAttributedRef` doesn't include the package object test.`package`,
// under joint and separate compilation.

package testSep { class C { object O } }
package testSep2 { object `package` { object PO; def bar = 0 } }
class DSep { object P }

object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -d " + testOutput.path
  override def sources = List(
    """
    package test { class C { object O } }
    class D { object P }
    package test2 { object `package` { object PO; def bar = 0 } }
    """
  )
  def check(source: String, unit: CompilationUnit) = enteringTyper {
    def checkTree(msg: String, t: => Tree) = {
      val run = currentRun
      import run._
      val phases = List(typerPhase, picklerPhase, refchecksPhase, uncurryPhase, specializePhase,
        explicitouterPhase, erasurePhase, posterasurePhase, flattenPhase, mixinPhase, cleanupPhase)
      for (phase <- phases) {
        enteringPhase(phase) {
          val error = t.exists(t => t.symbol == NoSymbol)
          val errorStr = if (error) "!!!" else " - "
          println(f"$phase%18s [$msg%12s] $errorStr $t")
        }
      }
      println("")
    }
    import rootMirror._

    println("\n\nJoint Compilation:\n")

    {
      val c = staticClass("test.C")
      val o = c.info.decl(TermName("O"))
      checkTree("O", gen.mkAttributedQualifier(o.moduleClass.thisType))
      val d = staticClass("D")
      val p = d.info.decl(TermName("P"))
      checkTree("P", gen.mkAttributedQualifier(p.moduleClass.thisType))
      val po = staticModule("test2.package").moduleClass.info.decl(TermName("PO"))
      checkTree("test2.PO", gen.mkAttributedQualifier(po.moduleClass.thisType))
      checkTree("test2.bar", gen.mkAttributedRef(po.owner.info.decl(TermName("bar"))))
    }

    println("\n\nSeparate Compilation:\n")

    {
      val c = typeOf[testSep.C].typeSymbol
      val o = c.info.decl(TermName("O"))
      checkTree("O", gen.mkAttributedQualifier(o.moduleClass.thisType))
      val d = staticClass("DSep")
      val p = d.info.decl(TermName("P"))
      checkTree("P", gen.mkAttributedQualifier(p.moduleClass.thisType))
      val po = staticModule("test2.package").moduleClass.info.decl(TermName("PO"))
      checkTree("PO", gen.mkAttributedQualifier(po.moduleClass.thisType))
      checkTree("testSep2.bar", gen.mkAttributedRef(po.owner.info.decl(TermName("bar"))))
    }
  }
}
