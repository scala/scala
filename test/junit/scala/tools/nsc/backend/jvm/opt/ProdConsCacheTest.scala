package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm.tree.MethodNode
import scala.tools.nsc.backend.jvm.analysis.ProdConsCache
import scala.tools.testkit.BytecodeTesting

/** The method-local optimizations analyze a method's producers and consumers only if they have a question for them. */
class ProdConsCacheTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._

  private def localOpt = global.genBCode.postProcessor.localOpt

  private def analyzedBy(source: String)(pass: (MethodNode, ProdConsCache) => Unit): Boolean = {
    val run = new global.Run()
    global.enteringPhase(run.jvmPhase) {
      global.genBCode.postProcessor.initialize()
      val method = compileAsmMethod(source, allowMessage = _.msg.contains("deprecation"))
      val cache = new ProdConsCache(method, "C")
      pass(method, cache)
      cache.isAnalyzed
    }
  }

  private def boxUnboxAnalyzes(source: String) =
    analyzedBy(source)((method, cache) => localOpt.boxUnbox.boxUnboxElimination(method, "C", cache))

  private def staleStoresAnalyzes(source: String) =
    analyzedBy(source)((method, cache) => localOpt.copyProp.eliminateStaleStoresAndRewriteSomeIntrinsics(method, "C", cache))

  @Test
  def boxUnboxOnlyForCreations(): Unit = {
    assertFalse(boxUnboxAnalyzes("def t(s: String): Int = s.trim.length + s.length"))
    assertFalse(boxUnboxAnalyzes("def t(s: String): Int = new StringBuilder(s).length + s.length"))
    for (creation <- List(
      "def t(i: Int): Any = i",
      "def t(i: Int): Any = java.lang.Integer.valueOf(i)",
      "def t(i: Int): Any = Predef.int2Integer(i)",
      "def t(i: Int): Any = new java.lang.Integer(i)",
      "def t(i: Int): Any = scala.runtime.IntRef.create(i)",
      "def t: Any = scala.runtime.IntRef.zero()",
      "def t(i: Int): Any = new scala.runtime.IntRef(i)",
      "def t(s: String): Any = (s, s)"))
      assertTrue(creation, boxUnboxAnalyzes(creation))
  }

  @Test
  def staleStoresOnlyForStoresAndClassTagNewArray(): Unit = {
    assertFalse(staleStoresAnalyzes("def t(s: String): Int = s.length"))
    assertTrue(staleStoresAnalyzes("def t(s: String): Int = { val l = s.length; l }"))
    assertTrue(staleStoresAnalyzes("def t(i: Int): Int = { var j = i; j += 1; j }"))
    assertTrue(staleStoresAnalyzes("def t(c: scala.reflect.ClassTag[String]): Any = c.newArray(1)"))
  }
}
