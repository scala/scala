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

  def errors(code: String, extraWconf: String = ""): List[Info] =
    reports(code, extraWconf).filter(_.severity == InternalReporter.ERROR)

  def reports(code: String, extraWconf: String = ""): List[Info] = {
    Wconf.clear()
    Wconf.tryToSet(WconfDefault)
    if (extraWconf.nonEmpty) Wconf.tryToSet(extraWconf.split(',').toList)
    val run = newRun()
    run.compileSources(makeSourceFile(code, "unitTestSource.scala") :: Nil)
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
      |  // not reported through reporting.uncheckedWarning, so not summarized, not subject to -unchecked
      |  def uncheckedWarningNotSummarized = Some(123).isInstanceOf[Option[Option[_]]]
      |
      |  class Outer {
      |    final case class UncheckedWarningSummarized(val s: String)
      |  }
      |
      |  @inline def nonFinal = 0
      |  def optimizerWarning = nonFinal
      |}
      |""".stripMargin

  val l4 = (4, "method f in class A is deprecated")
  val l6 = (6, "reflective access of structural type member method f should be enabled")
  val l8 = (8, "a pure expression does nothing in statement position")
  val l10 = (10, "fruitless type test: a value of type Some[List[Int]] cannot also be a Option[List[String]]")
  val l13 = (13, "non-variable type argument Option[_] in type Option[Option[_]] is unchecked since it is eliminated by erasure")
  val l16 = (16, "The outer reference in this type test cannot be checked at run time")
  val l20 = (20, "A::nonFinal()I is annotated @inline but could not be inlined")

  val s1 = (-1, "there was one deprecation warning")
  val s2 = (-1, "there was one feature warning")
  val s3 = (-1, "there was one optimizer warning")
  val s4 = (-1, "there was one unchecked warning")

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
    check(reports(code), List(s1, s2, s3, s4, l8, l10, l13))
  }

  @Test
  def noSummarizing(): Unit = {
    check(reports(code, "any:w"), List(l4, l6, l8, l10, l13, l16, l20))
  }

  @Test
  def silence(): Unit = {
    // TODO: directly reported, so not yet filtered
    check(reports(code, "any:s"), List(l8, l10, l13) /* should be Nil */)
  }

  @Test
  def deprecationSiteOrigin(): Unit = {
    check(errors(code, "site=A.f:e"), Nil)
    check(errors(code, "site=A.invokeDeprecated:e"), List(l4))
    check(errors(code, "origin=A.f:e"), List(l4))
    check(errors(code, "origin=A.invokeDeprecated:e"), Nil)
  }

  @Test
  def filterUnchecked(): Unit = {
    check(errors(code, "cat=unchecked:e"), List(l16))
  }
}
