package scala.tools.nsc
package settings

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testing.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class ScalaVersionTest {
  // SI-8711
  @Test def versionUnparse() {
    val v = "2.11.3"

    assertEquals(ScalaVersion(v).unparse, v)
  }
}
