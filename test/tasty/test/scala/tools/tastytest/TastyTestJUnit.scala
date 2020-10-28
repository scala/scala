package scala.tools.tastytest

import org.junit.{Ignore, Test => test}
import org.junit.Assert._

import scala.util.Properties

class TastyTestJUnit {

  @Ignore // https://github.com/scala/scala/pull/9280#issuecomment-717804739
  @test def run(): Unit = TastyTest.runSuite(
    src                     = "run",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def pos(): Unit = TastyTest.posSuite(
    src                     = "pos",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def posFalseNoAnnotations(): Unit = TastyTest.posSuite(
    src                     = "pos-false-noannotations",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Seq("-Ytasty-no-annotations"),
    additionalDottySettings = Nil
  ).get

  @Ignore // https://github.com/scala/scala/pull/9280#issuecomment-717804739
  @test def neg(): Unit = TastyTest.negSuite(
    src                     = "neg",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def negMoveMacros(): Unit = TastyTest.negChangePreSuite(
    src                     = "neg-move-macros",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDirs                 = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def negIsolated(): Unit = TastyTest.negSuiteIsolated(
    src                     = "neg-isolated",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDirs                 = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  val propSrc          = "tastytest.src"
  val propPkgName      = "tastytest.packageName"

  def assertPropIsSet(prop: String): String = {
    Properties.propOrNull(prop).ensuring(_ != null, s"-D$prop is not set")
  }
}
