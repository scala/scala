package scala.tools.xsbt

import org.junit.Test
import org.junit.Assert.assertEquals


class CodeActionTest extends BridgeTesting {
  @Test
  def procedureSyntax(): Unit = {
    withTemporaryDirectory { tempDir =>
      val reporter = mkReporter
      compileSrcs(tempDir, reporter, "object Foo { def foo { } }")
      val edit = reporter.messages.head.actions.get(0).edit().changes().get(0)
      assertEquals(20, edit.position().offset().get())
      assertEquals(": Unit =", edit.newText())
    }
  }
}
