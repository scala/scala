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

import java.net.URL

import scala.tools.nsc.ScalaDocReporter
import scala.tools.nsc.doc.Universe
import scala.tools.nsc.doc.html.Page
import scala.tools.nsc.doc.html.page.EntityPage
import scala.tools.nsc.doc.html.page.diagram.{DiagramGenerator, DotDiagramGenerator}
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "test/scaladoc/resources/doc-source-url.scala"

  override def model: Option[Universe] = newDocFactory.makeUniverse(Left(List(resourceFile)))

  def scaladocSettings = "-doc-source-url file:€{FILE_PATH}||€{FILE_EXT}||€{FILE_PATH_EXT}||€{FILE_LINE}"

  def testModel(rootPackage: Package) = {
    import access._

    val clazz = rootPackage._class("WithSource")

    val expect = s"file:test/scaladoc/resources/doc-source-url||.scala||test/scaladoc/resources/doc-source-url.scala||13"
    assert(clazz.sourceUrl.contains(new URL(expect)), s"got ${clazz.sourceUrl}")
  }
}
