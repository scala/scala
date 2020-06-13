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

import scala.tools.nsc.{ScalaDocReporter, doc, io}
import scala.tools.nsc.doc.DocFactory
import scala.tools.nsc.doc.model._
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  def destinationDir = "target/anyref"
  override def code = """class A"""
  override def scaladocSettings =
    s"-doc-no-compile src/library-aux -d ${destinationDir}"

  def testModel(rootPackage: Package): Unit = {
    import access._
    val t = rootPackage._package("scala")._class("AnyRef")

    val notifyAll = t._method("notifyAll")
    println(notifyAll.comment)

    val List(wait1, wait2, wait3) = t._methods("wait")
    println(List(wait1, wait2, wait3))
    println(wait1.comment)
    println(wait2.comment)
    println(wait3.comment)

    val sync = t._method("synchronized")
    println(sync.comment)
  }
}
