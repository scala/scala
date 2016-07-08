
package scala.util

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

/** The java version property uses the spec version
 *  and must work for legacy "major.minor" and plain version_number,
 *  and fail otherwise.
 */
@RunWith(classOf[JUnit4])
class SpecVersionTest {
  class TestProperties(versionAt: String) extends PropertiesTrait {
    override def javaSpecVersion = versionAt

    override protected def pickJarBasedOn: Class[_] = ???
    override protected def propCategory: String = "test"

    // override because of vals like releaseVersion
    override lazy val scalaProps = new java.util.Properties
  }
  val sut7 = new TestProperties("1.7")
  val sut9 = new TestProperties("9")

  @Test
  def comparesJDK9Correctly(): Unit = {
    assert(sut9 isJavaAtLeast "1")
    assert(sut9 isJavaAtLeast "1.5")
    assert(sut9 isJavaAtLeast "5")
    assert(sut9 isJavaAtLeast "1.8")
    assert(sut9 isJavaAtLeast "8")
    assert(sut9 isJavaAtLeast "9")
  }

  // SI-7265
  @Test
  def comparesCorrectly(): Unit = {
    assert(sut7 isJavaAtLeast "1")
    assert(sut7 isJavaAtLeast "1.5")
    assert(sut7 isJavaAtLeast "5")
    assert(sut7 isJavaAtLeast "1.6")
    assert(sut7 isJavaAtLeast "1.7")
    assertFalse(sut7 isJavaAtLeast "1.8")
    assertFalse(sut7 isJavaAtLeast "9")
    assertFalse(sut7 isJavaAtLeast "10")
  }

  @Test def variousBadVersionStrings(): Unit = {
    assertThrows[NumberFormatException] { sut7 isJavaAtLeast "1.9" }
    assertThrows[NumberFormatException] { sut9 isJavaAtLeast "1.9" }
    assertThrows[NumberFormatException] { sut7 isJavaAtLeast "9.1" }
    assertThrows[NumberFormatException] { sut9 isJavaAtLeast "9.1" }
  }

  @Test(expected = classOf[NumberFormatException])
  def badVersion(): Unit = {
    sut7 isJavaAtLeast "1.a"
  }
  @Test(expected = classOf[NumberFormatException])
  def noVersion(): Unit = {
    sut7 isJavaAtLeast ""
  }
  @Test(expected = classOf[NumberFormatException])
  def dotOnly(): Unit = {
    sut7 isJavaAtLeast "."
  }
  @Test(expected = classOf[NumberFormatException])
  def leadingDot(): Unit = {
    sut7 isJavaAtLeast ".5"
  }
  @Test(expected = classOf[NumberFormatException])
  def notASpec(): Unit = {
    sut7 isJavaAtLeast "1.7.1"
  }
}
