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

import org.junit.Test

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.{Reporter => InternalReporter}
import scala.tools.nsc.reporters.StoreReporter.Info
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class WConfTest extends BytecodeTesting {
  import compiler._

  override def compilerArgs: String = "-opt:l:inline -opt-inline-from:**"

  def Wconf = global.settings.Wconf
  val WconfDefault = cached("WConfDefault", () => Wconf.value)

  def errors(code: String, extraWconf: String = "", lint: Boolean = false): List[Info] =
    reports(code, extraWconf, lint).filter(_.severity == InternalReporter.ERROR)

  def infos(code: String, extraWconf: String = "", lint: Boolean = false): List[Info] =
    reports(code, extraWconf, lint).filter(_.severity == InternalReporter.INFO)

  def reports(code: String, extraWconf: String = "", lint: Boolean = false): List[Info] = {
    // lint has a postSetHook to enable `deprecated`, which in turn adds to `Wconf`,
    // but since we override Wconf after, that effect is cancelled
    if (lint) global.settings.lint.tryToSet(List("_"))
    Wconf.clear()
    Wconf.tryToSet(WconfDefault)
    if (extraWconf.nonEmpty) Wconf.tryToSet(extraWconf.split(',').toList)
    val run = newRun()
    run.compileSources(makeSourceFile(code, "unitTestSource.scala") :: Nil)
    if (lint) global.settings.lint.clear()
    global.reporter.asInstanceOf[StoreReporter].infos.toList.sortBy(p => (if (p.pos.isDefined) p.pos.point else -1, p.msg))
  }

  val code =
    """class A {
      |  @deprecated def f = 0
      |
      |  def invokeDeprecated = f
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
      |}
      |""".stripMargin

  val l2 = (2, "Specify both message and version: @deprecated(\"message\", since = \"1.0\")")
  val l4 = (4, "method f in class A is deprecated")
  val l6 = (6, "reflective access of structural type member method f should be enabled")
  val l8 = (8, "a pure expression does nothing in statement position")
  val l10 = (10, "fruitless type test: a value of type Some[List[Int]] cannot also be a Option[List[String]]")
  val l12 = (12, "non-variable type argument Option[_] in type Option[Option[_]] is unchecked since it is eliminated by erasure")
  val l15 = (15, "The outer reference in this type test cannot be checked at run time")
  val l19 = (19, "A::nonFinal()I is annotated @inline but could not be inlined")
  val l22 = (22, "adapted the argument list to the expected 2-tuple")

  val s1 = (-1, "there was one deprecation warning")
  val s2 = (-1, "there was one feature warning")
  val s3 = (-1, "there was one optimizer warning")
  val s4 = (-1, "there were two unchecked warnings")

  def check(actual: List[Info], expected: List[(Int, String)]): Unit = {
    def m(a: Info, e: (Int, String)) = a != null && e != null && (if (a.pos.isDefined) a.pos.line else -1) == e._1 && a.msg.startsWith(e._2)
    val ok = actual.zipAll(expected, null, null) forall {
      case (a, e) => m(a, e)
    }
    if (!ok) {
      val msg = ListBuffer.empty[String]
      actual
        .filterNot(a => expected.exists(e => m(a, e)))
        .foreach(a => msg += s"not expected: $a")
      expected
        .filterNot(e => actual.exists(a => m(a, e)))
        .foreach(e => msg += s"no actual for: $e")
      assert(false, s"\n-----------------\nactual:\n${actual.mkString("\n")}\n-----------------\nexpected:\n${expected.mkString("\n")}\n-----------------\n${msg.mkString("\n")}")
    }
  }

  @Test
  def default(): Unit = {
    check(reports(code), List(s1, s2, s3, l8, l10, l12, l15))
    check(reports(code, "cat=unchecked:ws"), List(s1, s2, s3, s4, l8, l10))
  }

  @Test
  def noSummarizing(): Unit = {
    check(reports(code, "any:w"), List(l4, l6, l8, l10, l12, l15, l19))
    check(reports(code, "any:w", lint = true), List(l2, l4, l6, l8, l10, l12, l15, l19, l22))
  }

  @Test
  def warnVerbose(): Unit = {
    check(reports(code, "any:wv"), List(
      l4.copy(_2 = "[deprecation @ A.invokeDeprecated | origin=A.f | version=] " + l4._2),
      l6.copy(_2 = "[feature-reflective-calls @ A.featureReflectiveCalls] " + l6._2),
      l8.copy(_2 = "[other-pure-statement @ A.pureExpressionAsStatement] " + l8._2),
      l10.copy(_2 = "[other @ A.fruitlessTypeTest] " + l10._2),
      l12.copy(_2 = "[unchecked @ A.uncheckedTypeTest] " + l12._2),
      l15.copy(_2 = "[unchecked @ A.Outer.UncheckedWarningSummarized.equals] " + l15._2),
      l19.copy(_2 = "[optimizer @ A.optimizerWarning] " + l19._2)))
  }

  @Test
  def silence(): Unit = {
    check(reports(code, "any:s"), Nil)
  }

  @Test
  def deprecationSiteOrigin(): Unit = {
    check(infos(code, "site=A.f:i"), Nil)
    check(infos(code, "site=A.invokeDeprecated:i"), List(l4))
    check(infos(code, "origin=A.f:i"), List(l4))
    check(infos(code, "origin=A.invokeDeprecated:i"), Nil)
  }

  @Test
  def filterUnchecked(): Unit = {
    check(infos(code, "cat=unchecked:i"), List(l12, l15))
  }

  @Test
  def featureWarnings(): Unit = {
    check(infos(code, "cat=feature:i"), List(l6))
    check(infos(code, "cat=feature-reflective-calls:i"), List(l6))
    check(infos(code, "cat=feature-higher-kinds:i"), Nil)
    check(infos(code, "cat=feature&site=A.*:i"), List(l6))
    check(infos(code, "cat=feature&site=A.featureReflectiveCalls:i"), List(l6))
    check(infos(code, "cat=unchecked&site=A.featureReflectiveCalls:i"), Nil)
    check(infos(code, "cat=feature&site=A.invokeDeprecated:i"), Nil)
  }

  @Test
  def lint(): Unit = {
    check(infos(code, "cat=lint:i"), Nil)
    check(infos(code, "cat=lint:i", lint = true), List(l2, l22))
    check(reports(code, "cat=lint:ws,any:s", lint = true), List((-1, "there were two lint warnings")))
    check(infos(code, "cat=lint-deprecation:i", lint = true), List(l2))
    check(infos(code, "cat=lint-adapted-args:i", lint = true), List(l22))
  }
}
