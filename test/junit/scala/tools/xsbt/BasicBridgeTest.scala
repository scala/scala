package scala.tools.xsbt

import org.junit.Test

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
}
