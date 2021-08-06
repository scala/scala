package scala.sys.process

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import scala.tools.testkit.AssertUtil._

class ProcessBuilderTest {

  @Test
  def t8406(): Unit = {
    import java.io.ByteArrayInputStream
    import java.nio.charset.StandardCharsets.UTF_8
    import scala.util.Try

    assertZeroNetThreads {
      val p = Try(("does-not-exist" #< new ByteArrayInputStream("foo".getBytes(UTF_8))).lazyLines)
      assertTrue(p.isSuccess)
      val q = p.map(_.toList)
      assertTrue(q.isFailure, "Expected realizing bad command to fail.")
    }
  }
}
