package scala.tools.tastytest

import org.junit.{Test => test}
import org.junit.Assert._

import scala.util.Properties

class TastyTestJUnit {

  @test def run(): Unit = TastyTest.runSuite(
    src                = "run",
    dottyLibrary       = assertPropIsSet(propDottyLibrary),
    srcRoot            = assertPropIsSet(propSrc),
    pkgName            = assertPropIsSet(propPkgName),
    outDir             = None,
    additionalSettings = Nil
  )

  @test def pos(): Unit = TastyTest.posSuite(
    src                = "pos",
    dottyLibrary       = assertPropIsSet(propDottyLibrary),
    srcRoot            = assertPropIsSet(propSrc),
    pkgName            = assertPropIsSet(propPkgName),
    outDir             = None,
    additionalSettings = Nil
  )

  @test def posFalse(): Unit = TastyTest.posSuite(
    src                = "pos-false",
    dottyLibrary       = assertPropIsSet(propDottyLibrary),
    srcRoot            = assertPropIsSet(propSrc),
    pkgName            = assertPropIsSet(propPkgName),
    outDir             = None,
    additionalSettings = Nil
  )

  @test def posFalseNoAnnotations(): Unit = TastyTest.posSuite(
    src                = "pos-false-noannotations",
    dottyLibrary       = assertPropIsSet(propDottyLibrary),
    srcRoot            = assertPropIsSet(propSrc),
    pkgName            = assertPropIsSet(propPkgName),
    outDir             = None,
    additionalSettings = Seq("-Ytasty-no-annotations")
  )

  @test def neg(): Unit = TastyTest.negSuite(
    src                = "neg",
    dottyLibrary       = assertPropIsSet(propDottyLibrary),
    srcRoot            = assertPropIsSet(propSrc),
    pkgName            = assertPropIsSet(propPkgName),
    outDir             = None,
    additionalSettings = Nil
  )

  @test def negFalse(): Unit = TastyTest.negSuite(
    src                = "neg-false",
    dottyLibrary       = assertPropIsSet(propDottyLibrary),
    srcRoot            = assertPropIsSet(propSrc),
    pkgName            = assertPropIsSet(propPkgName),
    outDir             = None,
    additionalSettings = Nil
  )

  val propDottyLibrary = "tastytest.dotty-library"
  val propSrc          = "tastytest.src"
  val propPkgName      = "tastytest.packageName"

  def assertPropIsSet(prop: String): String = {
    Properties.propOrNull(prop).ensuring(_ != null, s"-D$prop is not set")
  }
}
