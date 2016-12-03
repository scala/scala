package scala.sys.process

import java.io.ByteArrayInputStream
// should test from outside the package to ensure implicits work
//import scala.sys.process._
import scala.util.Properties._

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.assertEquals

@RunWith(classOf[JUnit4])
class ProcessTest {
  private def testily(body: => Unit) = if (!isWin) body
  @Test def t10007(): Unit = testily {
    val res = ("cat" #< new ByteArrayInputStream("lol".getBytes)).!!
    assertEquals("lol\n", res)
  }
  // test non-hanging
  @Test def t10055(): Unit = testily {
    val res = ("cat" #< ( () => -1 ) ).!
    assertEquals(0, res)
  }
}
