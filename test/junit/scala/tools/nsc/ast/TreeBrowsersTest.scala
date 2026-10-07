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

package scala
package tools.nsc
package ast

import annotation._
import reporters.StoreReporter
import reflect.internal.util.BatchSourceFile
import reflect.io.VirtualDirectory

import org.junit.Assert.assertFalse
import org.junit.{Test => test}

@nowarn("msg=early initializers")
class TreeBrowsersTest {
  abstract class TestTreeBrowsers extends TreeBrowsers {
    val global: TestGlobal
    import global._

    override def create(): SwingBrowser = new TestSwingBrowser()

    class TestSwingBrowser extends SwingBrowser {
      override def browse(pName: String, units: List[CompilationUnit]): Unit = {
        for (unit <- units)
          walkChildren(unit.body)
      }
      def walkChildren(tree: Tree): Unit = {
        for (child <- TreeInfo.children(tree))
          walkChildren(child)
      }
    }
  }
  class TestGlobal(settings: Settings, reporter: StoreReporter) extends Global(settings, reporter) {
    object testTreeBrowsers extends {
      val global: TestGlobal.this.type = TestGlobal.this
    } with TestTreeBrowsers
    override val treeBrowser = testTreeBrowsers.create().asInstanceOf[treeBrowsers.SwingBrowser]
  }
  val settings = new Settings
  val args = List("-Vbrowse:parser")
  settings.processArguments(args, processAll = true)
  val dir = new VirtualDirectory("test.out", maybeContainer = None)
  settings.outputDirs.setSingleOutput(dir)
  settings.usejavacp.value = true
  val reporter = new StoreReporter(settings)
  val global = new TestGlobal(settings, reporter)
  @test def children: Unit = {
    val code =
      sm"""
        |class C {
        |  def f(x: Int) = x
        |  def g = f(x = 42)
        |}
        """
    val src = new BatchSourceFile("bsf.scala", code)
    new global.Run().compileSources(src :: Nil)
    assertFalse(reporter.hasErrors)
  }
}
