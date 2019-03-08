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

  override def resourceFile: String = "test/scaladoc/resources/canonical.scala"

  def destinationDir = "target/canonical-unset"

  override def scaladocSettings =
    s"-d ${destinationDir}"

  override def code = ""

  def testModel(rootPackage: Package): Unit = {
    val dir = new java.io.File(destinationDir)
    dir.mkdirs()
    newDocFactory.document(List(resourceFile))
    val Pattern = """<link href="([^"]*)" rel="canonical" />""".r
    val s = io.File(s"${dir.getAbsolutePath}/p/Canonical.html").slurp()
    Pattern.findFirstIn(s) match {
      case Some(s) =>
        println(s)
      case _ =>
        println("As expected, no canonical URL found.")
    }
  }
}
