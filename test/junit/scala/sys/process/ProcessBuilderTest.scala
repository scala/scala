package scala.sys.process

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class ProcessBuilderTest {

  @Test
  def t8406: Unit = {
    import java.io.ByteArrayInputStream
    import scala.util.Try
    assert(Try(("does-not-exist" read new ByteArrayInputStream("foo".getBytes("utf-8"))).lazyLines).isSuccess)
    assert(Try(("does-not-exist" read new ByteArrayInputStream("foo".getBytes("utf-8"))).lazyLines.toList).isFailure)
  }
}
