package scala.tools.xsbt

import org.junit.Test
import xsbti.CompileFailed

import scala.tools.testkit.AssertUtil.assertThrown

class BasicBridgeTest extends BridgeTesting {
  @Test
  def bridgeCompiles(): Unit = {
    withTemporaryDirectory { tempDir =>
      compileSrcs(tempDir, "object Foo")
      val t = tempDir / "target" / "Foo$.class"
      assert(t.exists)
    }
  }

  @Test
  def bridgeDocs(): Unit = {
    withTemporaryDirectory { tempDir =>
      docSrcs(tempDir.toPath)
      val t = tempDir / "target" / "index.html"
      assert(t.exists)
    }
  }

  @Test
  def noNPEonVphases(): Unit = withTemporaryDirectory { tempDir =>
    val compiler = mkCompiler
    assertThrown[CompileFailed](
      _.toString.contains("Compiler option supplied that disabled Zinc compilation"))(
      compiler.run(
        sources = Array.empty,
        changes = emptyChanges,
        options = Array("-usejavacp", "-Vphases"),
        output = new TestOutput(tempDir),
        callback = new TestCallback,
        delegate = mkReporter,
        progress = ignoreProgress,
        log = TestLogger))
  }
}
