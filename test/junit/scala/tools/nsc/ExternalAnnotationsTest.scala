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

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.language.implicitConversions
import scala.reflect.internal.Reporter
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.BytecodeTesting

class ExternalAnnotationsTest extends BytecodeTesting {
  import compiler._
  import global._

  def parse(extAnns: String, nbMessages: Int = 0, allowMessage: StoreReporter.Info => Boolean = _ => false): List[ExtAnn] = {
    newRun()
    val r = global.parseExternalAnnotations(extAnns, "<external-annotations>")
    runReporting.reportSuspendedMessages()
    assertEquals(nbMessages, global.reporter.asInstanceOf[StoreReporter].infos.size)
    compiler.checkReport(allowMessage)
    r
  }

  implicit def toNameInt(sc: StringContext) = new NameInt(sc)
  class NameInt(sc: StringContext) {
    def tm(args: Any*): TermName = TermName(sc.parts(0))
    def tp(args: Any*): TypeName = TypeName(sc.parts(0))
  }

  // Only warnings from external annotations, no errors
  def checkMessage(s: String) = (i: StoreReporter.Info) => i.severity == Reporter.WARNING && i.msg.contains(s)

  @Test
  def parsing(): Unit = {
    val r = parse(
      """@a1
        |@a2("arg", 1) @a3(param = 0) // multiple annotations on a line
        |C
        |a.b.C.d
        |
        |@a4
        |a$.`b`$.C#.`D`#.`e`         // default term / type
        |a#.C$.`x#`.`D$`.`k$`#.`U#`$ // change term / type
        |x.y._
        |
        |@a5 p.`::` u.`###` v.`::`#  // annotation and multiple targets on a line
        |""".stripMargin,
      1, checkMessage("named arguments not supported"))
    assertEquals(
      List(
        ExtAnn(List(tp"C"), "a1", List()),
        ExtAnn(List(tp"C"), "a2", List("arg", 1)),
        ExtAnn(List(tp"C"), "a3", List(0)),
        ExtAnn(List(tm"d", tp"C", tm"b", tm"a"), "a1" , List()),
        ExtAnn(List(tm"d", tp"C", tm"b", tm"a"), "a2" , List("arg", 1)),
        ExtAnn(List(tm"d", tp"C", tm"b", tm"a"), "a3" , List(0)),
        ExtAnn(List(tm"e", tp"D", tp"C", tm"b", tm"a"), "a4", List()),
        ExtAnn(List(tm"U$$hash", tp"k$$", tp"D$$", tm"x$$hash", tm"C", tp"a"), "a4", List()),
        ExtAnn(List(tm"_", tm"y", tm"x"), "a4", List()),
        ExtAnn(List(tm"$$colon$$colon", tm"p"), "a5", List()),
        ExtAnn(List(tm"$$hash$$hash$$hash", tm"u"), "a5", List()),
        ExtAnn(List(tp"$$colon$$colon", tm"v"), "a5", List())),
      r)
  }

  @Test
  def noTarget(): Unit = {
    parse(
      """@a1 @a2("hai")
        |""".stripMargin, 1, checkMessage("no annotation targets found"))
  }

  @Test
  def unexpectedToken(): Unit = {
    val m = "invalid syntax in external annotation, unexpected 'def'"
    parse(
      """@a x.P
        |@b y.C def.D
        |""".stripMargin,
      1, checkMessage(m))
    parse(
      """@a x.P.def.U
        |""".stripMargin,
      1, checkMessage(m))
  }

  @Test
  def invlidAnnots(): Unit = {
    parse(
      """@a.def.foo x
        |""".stripMargin,
      1, checkMessage("identifier expected but 'def' found"))

    parse(
      """@a(foo) x
        |""".stripMargin,
      1, checkMessage("annotation arguments need to be constants"))

    parse(
      """@a(2 + 3) x
        |""".stripMargin,
      1, checkMessage("annotation arguments need to be constants"))
  }
}
