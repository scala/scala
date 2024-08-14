/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package reporters

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import java.io.{File => JFile}

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.internal.{Reporter => InternalReporter}
import scala.reflect.io.PlainFile
import scala.tools.nsc.Reporting.{Version, WarningCategory}
import scala.reflect.io.File
import scala.tools.nsc.reporters.StoreReporter.Info
import scala.tools.testkit.AssertUtil.fail
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class WConfTest extends BytecodeTesting {
  import compiler._

  override def compilerArgs: String = "-opt:inline:**"

  def Wconf = global.settings.Wconf
  val WconfDefault = cached("WConfDefault", () => Wconf.value)

  def errors(code: String, extraWconf: String = "", lint: Boolean = false): List[Info] =
    reports(code, extraWconf, lint).filter(_.severity == InternalReporter.ERROR)

  def infos(code: String, extraWconf: String = "", lint: Boolean = false): List[Info] =
    reports(code, extraWconf, lint).filter(_.severity == InternalReporter.INFO)

  def reports(code: String, extraWconf: String = "", lint: Boolean = false): List[Info] = {
    global.settings.deprecation.reset()
    if (lint) {
      global.settings.warnUnused.clear()
      global.settings.lint.tryToSet(List("_"))
    }
    Wconf.clear()
    Wconf.tryToSet(WconfDefault)
    if (extraWconf.nonEmpty) Wconf.tryToSet(extraWconf.split(',').toList)
    val run = newRun()
    run.compileSources(makeSourceFile(code, "unitTestSource.scala") :: Nil)
    if (lint) {
      global.settings.lint.clear()
      global.settings.warnUnused.clear()
    }
    global.reporter.asInstanceOf[StoreReporter].infos.toList.sortBy(p => (if (p.pos.isDefined) p.pos.point else -1, p.msg))
  }

  val code =
    """class A {
      |  @deprecated def f = 0
      |  @deprecated("message", "my specs2 1.2.3-custom") def g = 0
      |
      |  def invokeDeprecated = f + g
      |
      |  def featureReflectiveCalls(a: { def f: Int }) = a.f
      |
      |  def pureExpressionAsStatement = {1; 2}
      |
      |  def fruitlessTypeTest = Some(List(1)).isInstanceOf[Option[List[String]]]
      |
      |  def uncheckedTypeTest = Some(123).isInstanceOf[Option[Option[_]]]
      |
      |  class Outer {
      |    final case class UncheckedWarningSummarized(val s: String)
      |  }
      |
      |  @inline def nonFinal = 0
      |  def optimizerWarning = nonFinal
      |
      |  def tupleMethod(a: (Int, Int)) = 0
      |  def lintAdaptedArgs = tupleMethod(1, 2)
      |
      |  def unusedLocal = { val h = hashCode; 1 }
      |}
      |""".stripMargin

  val l2 = (2, "Specify both message and version: @deprecated(\"message\", since = \"MyLib 1.0\")")
  val l5a = (5, "method f in class A is deprecated")
  val l5b = (5, "method g in class A is deprecated")
  val l7 = (7, "reflective access of structural type member method f should be enabled")
  val l9 = (9, "a pure expression does nothing in statement position")
  val l11 = (11, "fruitless type test: a value of type Some[List[Int]] cannot also be a Option[List[String]]")
  val l13 = (13, "non-variable type argument Option[_] in type Option[Option[_]] is unchecked since it is eliminated by erasure")
  val l16 = (16, "The outer reference in this type test cannot be checked at run time")
  val l20 = (20, "A::nonFinal()I is annotated @inline but could not be inlined")
  val l23 = (23, "adapted the argument list to the expected 2-tuple")
  val l25 = (25, "local val h in method unusedLocal is never used")

  val s1a = (-1, "1 deprecation")
  val s1b = (-1, "1 deprecation (since my specs2 1.2.3-custom)")
  val s1c = (-1, "2 deprecations in total")
  val s2 = (-1, "1 feature warning")
  val s3 = (-1, "1 optimizer warning")
  val s4 = (-1, "2 unchecked warnings")

  // check actual reports that line number matches and message contains the text
  def check(actual: List[Info], expected: List[(Int, String)]): Unit = {
    def m(a: Info, e: (Int, String)) = a != null && e != null && (if (a.pos.isDefined) a.pos.line else -1) == e._1 && a.msg.contains(e._2)
    val ok = actual.zipAll(expected, null, null) forall {
      case (a, e) => m(a, e)
    }
    if (!ok) {
      val msg = ListBuffer.empty[String]
      actual
        .filterNot(a => expected.exists(e => m(a, e)))
        .foreach(a => msg += s"not expected: (${a.pos.line}, ${a.msg})")
      expected
        .filterNot(e => actual.exists(a => m(a, e)))
        .foreach(e => msg += s"no actual for: $e")
      fail(s"\n-----------------\nactual:\n${actual.mkString("\n")}\n-----------------\nexpected:\n${expected.mkString("\n")}\n-----------------\n${msg.mkString("\n")}")
    }
  }

  @Test
  def default(): Unit = {
    check(reports(code), List(s1a, s1b, s2, s3, s1c, l9, l11, l13, l16))
    check(reports(code, "cat=unchecked:ws"), List(s1a, s1b, s2, s3, s1c, s4, l9, l11))
  }

  @Test
  def noSummarizing(): Unit = {
    check(reports(code, "any:w"), List(l5a, l5b, l7, l9, l11, l13, l16, l20))
    check(reports(code, "any:w", lint = true), List(l2, l5a, l5b, l7, l9, l11, l13, l16, l20, l23, l25))
  }

  @Test
  def warnVerbose: Unit =
    check(reports(code, "any:wv"), List(
      l5a.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=deprecation, site=A.invokeDeprecated, origin=A.f"),
      l5b.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=deprecation, site=A.invokeDeprecated, origin=A.g, version=1.2.3"),
      l7.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=feature-reflective-calls, site=A.featureReflectiveCalls"),
      l9.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=other-pure-statement, site=A.pureExpressionAsStatement"),
      l11.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=other, site=A.fruitlessTypeTest"),
      l13.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=unchecked, site=A.uncheckedTypeTest"),
      l16.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=unchecked, site=A.Outer.UncheckedWarningSummarized.equals"),
      l20.copy(_2 = "\nApplicable -Wconf / @nowarn filters for this warning: msg=<part of the message>, cat=optimizer, site=A.optimizerWarning"),
      )
    )

  @Test
  def silence(): Unit = {
    check(reports(code, "any:s"), Nil)
  }

  @Test
  def deprecationSiteOrigin(): Unit = {
    check(infos(code, "site=A.f:i"), Nil)
    check(infos(code, "site=A.invokeDeprecated:i"), List(l5a, l5b))
    check(infos(code, "origin=A.f:i"), List(l5a))
    check(infos(code, "origin=A.invokeDeprecated:i"), Nil)
  }

  @Test
  def deprecatedSince(): Unit = {
    check(infos(code, "cat=deprecation:i"), List(l5a, l5b))
    check(infos(code, "origin=A.[fg]:i"), List(l5a, l5b))

    check(infos(code, "since=1.2.3:i"), List(l5b))
    check(infos(code, "since=1.2:i"), Nil)
    check(infos(code, "since=1:i"), Nil)

    check(infos(code, "since>1:i"), List(l5b))
    check(infos(code, "since>1.2:i"), List(l5b))
    check(infos(code, "since>1.2.1:i"), List(l5b))
    check(infos(code, "since>1.2.2:i"), List(l5b))
    check(infos(code, "since>1.2.3:i"), Nil)
    check(infos(code, "since>1.3:i"), Nil)
    check(infos(code, "since>2:i"), Nil)

    check(infos(code, "since<1.2.4:i"), List(l5b))
    check(infos(code, "since<1.3.0:i"), List(l5b))
    check(infos(code, "since<1.3:i"), List(l5b))
    check(infos(code, "since<2:i"), List(l5b))
    check(infos(code, "since<2.3:i"), List(l5b))
    check(infos(code, "since<2.3.4:i"), List(l5b))
    check(infos(code, "since<1.2.3:i"), Nil)
    check(infos(code, "since<1.2:i"), Nil)
    check(infos(code, "since<1:i"), Nil)

  }

  @Test
  def filterUnchecked(): Unit = {
    check(infos(code, "cat=unchecked:i"), List(l13, l16))
  }

  @Test
  def featureWarnings(): Unit = {
    check(infos(code, "cat=feature:i"), List(l7))
    check(infos(code, "cat=feature-reflective-calls:i"), List(l7))
    check(infos(code, "cat=feature-higher-kinds:i"), Nil)
    check(infos(code, "cat=feature&site=A.*:i"), List(l7))
    check(infos(code, "cat=feature&site=A.featureReflectiveCalls:i"), List(l7))
    check(infos(code, "cat=unchecked&site=A.featureReflectiveCalls:i"), Nil)
    check(infos(code, "cat=feature&site=A.invokeDeprecated:i"), Nil)
  }

  @Test
  def lint(): Unit = {
    check(infos(code, "cat=lint:i"), Nil)
    check(infos(code, "cat=lint:i", lint = true), List(l2, l23))
    check(reports(code, "any:s,cat=lint:ws", lint = true), List((-1, "2 lint warnings")))
    check(reports(code, "cat=lint:ws,any:s", lint = true), Nil)
    check(infos(code, "cat=lint-deprecation:i", lint = true), List(l2))
    check(infos(code, "cat=lint-adapted-args:i", lint = true), List(l23))
  }

  @Test
  def messageFilters(): Unit = {
    check(errors(code, "msg=(b"), (-1, "no filters or no action defined") :: Nil)
    check(errors(code, "msg=(b:i"), (-1, "invalid pattern `(b`: Unclosed group near index 2") :: Nil)
    check(infos(code, "msg=method:i"), List(l5a, l5b, l7, l20))
    check(infos(code, "msg=.*is deprecated.*:i"), List(l5a, l5b))
    check(infos(code, "msg=is deprecated$:i"), List(l5a))
    check(infos(code, "msg=^is deprecated:i"), Nil)
  }

  @Test
  def unusedSite(): Unit = {
    check(infos(code, "cat=unused:iv", lint = true), (25, "local val h in method unusedLocal is never used\nApplicable -Wconf / @nowarn filters for this message: msg=<part of the message>, cat=unused-locals, site=A.unusedLocal.h") :: Nil)
    check(errors(code, "site=A\\.unusedLocal\\..*:e", lint = true), l25 :: Nil)
  }

  @Test
  def parseVersion(): Unit = {
    def check(s: String, ev: Version): Unit = Version.fromString(s) match {
      case pv: Version.ParseableVersion =>
        assertEquals(ev, pv.copy(orig = ""))
      case _: Version.NonParseableVersion =>
        assertTrue(s"failed to parse $s, expected $ev", ev.isInstanceOf[Version.NonParseableVersion])
    }

    val v123 = Version.ParseableVersion("", 1, Some(2), Some(3))
    val v12 = Version.ParseableVersion("", 1, Some(2), None)
    val v1 = Version.ParseableVersion("", 1, None, None)
    val nv = Version.NonParseableVersion("")

    check("1.2.3", v123)
    check("1.2.3m", v12)
    check("1.2x.3", v1)
    check("my library 1.2.3-patch-44.0", v123)
    check("specs2-33 1.2.3.10-patch-44.0", v123)
    check("   1.2.3.10-patch-44.0 has a tail", v123)
    check("knut 1.2-m3.10-patch-44.0 has a tail", v12)
    check("knut 1 am 1.2.3 so", v1)
    check("spec2.2", nv)
    check("only text, 1here", nv)
    check("", nv)
  }

  @Test
  def versionComparison(): Unit = {
    val s = (a: Version.ParseableVersion, b: Version.ParseableVersion) => a.smaller(b)
    val e = (a: Version.ParseableVersion, b: Version.ParseableVersion) => a.same(b)
    val g = (a: Version.ParseableVersion, b: Version.ParseableVersion) => a.greater(b)

    val ns = (a: Version.ParseableVersion, b: Version.ParseableVersion) => !a.smaller(b)
    val ne = (a: Version.ParseableVersion, b: Version.ParseableVersion) => !a.same(b)
    val ng = (a: Version.ParseableVersion, b: Version.ParseableVersion) => !a.greater(b)

    val name = Map(s -> "!<", e -> "!=", g -> "!>", ns -> "<", ne -> "=", ng -> ">")

    def check(a: Version.ParseableVersion, b: Version.ParseableVersion, op: (Version.ParseableVersion, Version.ParseableVersion) => Boolean): Unit =
      assertTrue(s"$a ${name(op)} $b", op(a, b))

    object v {
      def apply(maj: Int) = Version.ParseableVersion("", maj, None, None)
      def apply(maj: Int, min: Int) = Version.ParseableVersion("", maj, Some(min), None)
      def apply(maj: Int, min: Int, pat: Int) = Version.ParseableVersion("", maj, Some(min), Some(pat))
    }

    val v1 = v(1)
    val v12 = v(1, 2)
    val v123 = v(1,2,3)
    val v124 = v(1,2,4)

    val v10 = v(1, 0)
    val v100 = v(1, 0, 0)

    check(v1, v1, ns)
    check(v1, v1, e)
    check(v1, v1, ng)

    check(v1, v12, s)
    check(v1, v12, ne)
    check(v1, v12, ng)

    check(v1, v123, s)
    check(v1, v123, ne)
    check(v1, v123, ng)

    check(v1, v10, ns)
    check(v1, v10, e)
    check(v1, v10, ng)

    check(v1, v100, ns)
    check(v1, v100, e)
    check(v1, v100, ng)

    check(v123, v1, ns)
    check(v123, v1, ne)
    check(v123, v1, g)

    check(v123, v12, ns)
    check(v123, v12, ne)
    check(v123, v12, g)

    check(v123, v123, ns)
    check(v123, v123, e)
    check(v123, v123, ng)

    check(v123, v124, s)
    check(v123, v124, ne)
    check(v123, v124, ng)
  }

  @Test
  def sourcePatternTest(): Unit = {
    def m(p: String) = Reporting.Message.Plain(
      Position.offset(new BatchSourceFile(new PlainFile(new File(new JFile(p))) {
        override lazy val canonicalPath: String = p
      }, Array().toIndexedSeq), 0),
      msg = "",
      WarningCategory.Other,
      site = "",
      actions = Nil)

    val aTest = Reporting.WConf.parseFilter("src=a/.*Test.scala", rootDir = "").getOrElse(null)
    assertTrue(aTest.matches(m("/a/FooTest.scala")))
    assertTrue(aTest.matches(m("/x/a/b/FooTest.scala")))
    assertTrue(aTest.matches(m("c:\\my user\\a\\Test.scala")))
    assertTrue(!aTest.matches(m("/x/mama/Test.scala")))

    val noScala = Reporting.WConf.parseFilter("src=a/.*Test", rootDir = "").getOrElse(null)
    assertTrue(!noScala.matches(m("/x/a/FooTest.scala")))

    val withRootDir = Reporting.WConf.parseFilter("src=a/.*Test.scala", rootDir = "/root/path").getOrElse(null)
    assertTrue(withRootDir.matches(m("/root/path/a/FooTest.scala")))
    assertTrue(withRootDir.matches(m("/root/path/a/b/c/FooTest.scala")))
    assertTrue(!withRootDir.matches(m("/root/path/xa/FooTest.scala")))
    assertTrue(!withRootDir.matches(m("/root/a/FooTest.scala")))
    assertTrue(!withRootDir.matches(m("/root/path/b/a/FooTest.scala")))

    val withRootDirWin = Reporting.WConf.parseFilter("src=a/.*Test.scala", rootDir = "c:/root/path").getOrElse(null)
    assertTrue(withRootDirWin.matches(m("c:/root/path/a/FooTest.scala")))
    assertTrue(withRootDirWin.matches(m("c:/root/path/a/b/c/FooTest.scala")))
    assertTrue(!withRootDirWin.matches(m("c:/root/path/xa/FooTest.scala")))
    assertTrue(!withRootDirWin.matches(m("c:/root/a/FooTest.scala")))
    assertTrue(!withRootDirWin.matches(m("c:/root/path/b/a/FooTest.scala")))
  }
}
