package scala.tools.tastytest

import org.junit.Test
import org.junit.Assert._

import scala.util.Properties

class TastyTestJUnit {

  @Test def run(): Unit = TastyTest.runSuite(
    dottyLibrary = assertPropIsSet(propDottyLibrary),
    srcRoot      = assertPropIsSet(propSrc),
    pkgName      = assertPropIsSet(propPkgName),
    outDir       = None
  ).get

  @Test def neg(): Unit = TastyTest.negSuite(
    dottyLibrary = assertPropIsSet(propDottyLibrary),
    srcRoot      = assertPropIsSet(propSrc),
    pkgName      = assertPropIsSet(propPkgName),
    outDir       = None
  ).get

  val propDottyLibrary = "tastytest.dotty-library"
  val propSrc          = "tastytest.src"
  val propPkgName      = "tastytest.packageName"

  def assertPropIsSet(prop: String): String = {
    assert(Properties.propIsSet(prop), s"-D$prop is not set")
    Properties.propOrEmpty(prop)
  }
}
