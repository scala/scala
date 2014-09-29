
package scala.util

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** The java version property uses the spec version
 *  and must work for all "major.minor" and fail otherwise.
 */
@RunWith(classOf[JUnit4])
class SpecVersionTest {
  val sut = new PropertiesTrait {
    override def javaSpecVersion = "1.7"

    override protected def pickJarBasedOn: Class[_] = ???
    override protected def propCategory: String = "test"

    // override because of vals like releaseVersion
    override lazy val scalaProps = new java.util.Properties
  }

  // SI-7265
  @Test
  def comparesCorrectly(): Unit = {
    assert(sut isJavaAtLeast "1.5")
    assert(sut isJavaAtLeast "1.6")
    assert(sut isJavaAtLeast "1.7")
    assert(!(sut isJavaAtLeast "1.8"))
    assert(!(sut isJavaAtLeast "1.71"))
  }
  @Test(expected = classOf[NumberFormatException])
  def badVersion(): Unit = {
    sut isJavaAtLeast "1.a"
  }
  @Test(expected = classOf[NumberFormatException])
  def missingVersion(): Unit = {
    sut isJavaAtLeast "1"
  }
  @Test(expected = classOf[NumberFormatException])
  def noVersion(): Unit = {
    sut isJavaAtLeast ""
  }
  @Test(expected = classOf[NumberFormatException])
  def dotOnly(): Unit = {
    sut isJavaAtLeast "."
  }
  @Test(expected = classOf[NumberFormatException])
  def leadingDot(): Unit = {
    sut isJavaAtLeast ".5"
  }
  @Test(expected = classOf[NumberFormatException])
  def notASpec(): Unit = {
    sut isJavaAtLeast "1.7.1"
  }
}
