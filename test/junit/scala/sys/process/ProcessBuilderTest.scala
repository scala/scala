package scala.sys.process

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._
import scala.tools.testkit.AssertUtil._

@RunWith(classOf[JUnit4])
class ProcessBuilderTest {

  @Test
  def t8406: Unit = {
    import java.io.ByteArrayInputStream
    import java.nio.charset.StandardCharsets.UTF_8
    import scala.util.Try

    assertZeroNetThreads {
      val p = Try(("does-not-exist" #< new ByteArrayInputStream("foo".getBytes(UTF_8))).lazyLines)
      assertTrue(p.isSuccess)
      val q = p.map(_.toList)
      assertTrue("Expected realizing bad command to fail.", q.isFailure)
    }
  }
}
