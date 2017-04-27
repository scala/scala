
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

  @Test
  def comparesJDK9Correctly(): Unit = {
    val sut9 = new TestProperties("9")
    assert(sut9 isJavaAtLeast "1")
    assert(sut9 isJavaAtLeast "1.5")
    assert(sut9 isJavaAtLeast "5")
    assert(sut9 isJavaAtLeast "1.8")
    assert(sut9 isJavaAtLeast "8")
    assert(sut9 isJavaAtLeast "9")
    assertFalse(sut9.isJavaAtLeast("10"))
  }

  // scala/bug#7265
  @Test
  def comparesCorrectly(): Unit = {
    val sut7 = new TestProperties("1.7")
    assert(sut7 isJavaAtLeast "1")
    assert(sut7 isJavaAtLeast "1.5")
    assert(sut7 isJavaAtLeast "5")
    assert(sut7 isJavaAtLeast "1.6")
    assert(sut7 isJavaAtLeast "1.7")
    assert(sut7.isJavaAtLeast("7"))
    assertFalse(sut7.isJavaAtLeast("9"))
    assertFalse(sut7 isJavaAtLeast "1.8")
    assertFalse(sut7 isJavaAtLeast "9")
    assertFalse(sut7 isJavaAtLeast "10")
  }

  @Test def variousBadVersionStrings(): Unit = {
    val sut = new TestProperties("9")
    assertThrows[NumberFormatException](sut.isJavaAtLeast("1.9"), _ == "Not a version: 1.9")
    assertThrows[NumberFormatException](sut.isJavaAtLeast("1."))
    assertThrows[NumberFormatException](sut.isJavaAtLeast("1.8."))
    assertThrows[NumberFormatException](sut.isJavaAtLeast("1.a"))
    assertThrows[NumberFormatException](sut.isJavaAtLeast(""))
    assertThrows[NumberFormatException](sut.isJavaAtLeast("."))
    assertThrows[NumberFormatException](sut.isJavaAtLeast(".."))
    assertThrows[NumberFormatException](sut.isJavaAtLeast(".5"))
    assertThrows[NumberFormatException](sut.isJavaAtLeast("9-ea"))  //version number, not version string
  }

  @Test def `spec has minor or more`(): Unit = {
    val sut = new TestProperties("9.2.5")
    assert(sut.isJavaAtLeast("9"))
    assert(sut.isJavaAtLeast("9.0.1"))
    assert(sut.isJavaAtLeast("9.2.1"))
    assert(sut.isJavaAtLeast("8.3.1"))
    assert(sut.isJavaAtLeast("8.3.1.1.1"))
    assertFalse(sut.isJavaAtLeast("9.3.1"))
    assertFalse(sut.isJavaAtLeast("10.3.1"))
  }

  @Test def `compares only major minor security`(): Unit = {
    val sut = new TestProperties("9.2.5.1.2.3")
    assert(sut.isJavaAtLeast("9"))
    assert(sut.isJavaAtLeast("9.0.1"))
    assert(sut.isJavaAtLeast("9.2.5.9.9.9"))
    assertFalse(sut.isJavaAtLeast("9.2.6"))
  }

  @Test def `futurely proofed`(): Unit = {
    val sut = new TestProperties("10.2.5")
    assert(sut.isJavaAtLeast("10"))
    assert(sut.isJavaAtLeast("9"))
    assert(sut.isJavaAtLeast("9.0.1"))
    assert(sut.isJavaAtLeast("9.2.1"))
    assert(sut.isJavaAtLeast("8.3.1"))
    assert(sut.isJavaAtLeast("8.3.1.1.1"))
    assert(sut.isJavaAtLeast("9.3.1"))
    assertFalse(sut.isJavaAtLeast("10.3.1"))
  }
}
