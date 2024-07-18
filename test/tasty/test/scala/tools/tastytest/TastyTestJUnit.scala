package scala.tools.tastytest

import org.junit.{Test => test, BeforeClass => setup, AfterClass => teardown}
import org.junit.Assert._

import scala.util.Properties

import TastyTestJUnit._

class TastyTestJUnit {

  @test def run(): Unit = TastyTest.runSuite(
    src                     = "run",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def runPipelined(): Unit = {
    TastyTest.runPipelinedSuite(
      src                     = "run-pipelined",
      srcRoot                 = assertPropIsSet(propSrc),
      pkgName                 = assertPropIsSet(propPkgName),
      outDirs                 = None,
      additionalSettings      = Nil,
      additionalDottySettings = Seq("-Yexplicit-nulls"), // test flexible types from Java
    ).get
  }

  @test def pos(): Unit = TastyTest.posSuite(
    src                     = "pos",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  /** false positives that should fail, but work when annotations are not read */
  @test def posFalseNoAnnotations(): Unit = TastyTest.posSuite(
    src                     = "pos-false-noannotations",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Seq("-Ytasty-no-annotations"),
    additionalDottySettings = Nil
  ).get

  @test def posDoctool(): Unit = TastyTest.posDocSuite(
    src                     = "pos-doctool",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def neg(): Unit = TastyTest.negSuite(
    src                     = "neg",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  @test def negPipelined(): Unit = {
    TastyTest.negPipelinedSuite(
      src                     = "neg-pipelined",
      srcRoot                 = assertPropIsSet(propSrc),
      pkgName                 = assertPropIsSet(propPkgName),
      outDirs                 = None,
      additionalSettings      = Nil,
      additionalDottySettings = Seq("-Yexplicit-nulls") // test flexible types from Java
    ).get
  }

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

  @test def negFullCircle(): Unit = TastyTest.negFullCircleSuite(
    src                     = "neg-full-circle",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).get

  val propSrc     = "tastytest.src"
  val propPkgName = "tastytest.packageName"

  def assertPropIsSet(prop: String): String = {
    Properties.propOrElse(prop, {
      fail(s"-D$prop is not set")
      "(unknown)"
    })
  }
}

object TastyTestJUnit {

  private[this] var _dottyClassLoader: Dotc.ClassLoader = _
  implicit def dottyClassLoader: Dotc.ClassLoader = _dottyClassLoader

  @setup
  def init(): Unit = {
    _dottyClassLoader = Dotc.initClassloader().get
  }

  @teardown
  def finish(): Unit = {
    _dottyClassLoader = null
  }
}
