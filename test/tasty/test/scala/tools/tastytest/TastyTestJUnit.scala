package scala.tools.tastytest

import org.junit.{Test => test, BeforeClass => setup, AfterClass => teardown}
import org.junit.Assert._

import scala.util.{Try, Failure, Properties}

import TastyTestJUnit._

class TastyTestJUnit {

  @test def run(): Unit = TastyTest.runSuite(
    src                     = "run",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).eval

  @test def pos(): Unit = TastyTest.posSuite(
    src                     = "pos",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).eval

  @test def posFalseNoAnnotations(): Unit = TastyTest.posSuite(
    src                     = "pos-false-noannotations",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Seq("-Ytasty-no-annotations"),
    additionalDottySettings = Nil
  ).eval

  @test def neg(): Unit = TastyTest.negSuite(
    src                     = "neg",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDir                  = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).eval

  @test def negMoveMacros(): Unit = TastyTest.negChangePreSuite(
    src                     = "neg-move-macros",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDirs                 = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).eval

  @test def negIsolated(): Unit = TastyTest.negSuiteIsolated(
    src                     = "neg-isolated",
    srcRoot                 = assertPropIsSet(propSrc),
    pkgName                 = assertPropIsSet(propPkgName),
    outDirs                 = None,
    additionalSettings      = Nil,
    additionalDottySettings = Nil
  ).eval

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

  final implicit class TryOps(val op: Try[Unit]) extends AnyVal {
    def eval: Unit = op match {
      case Failure(err) => fail(err.toString)
      case _ => ()
    }
  }
}
