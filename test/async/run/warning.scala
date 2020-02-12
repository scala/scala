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

package scala.async
package run

import org.junit.Test

import scala.language.{postfixOps, reflectiveCalls}
import scala.tools.nsc.reporters.StoreReporter


class WarningsSpec {

  @Test
  // https://github.com/scala/async/issues/74
  def noPureExpressionInStatementPositionWarning_t74(): Unit = {
    val tb = mkToolbox(s"-cp ${toolboxClasspath} -Xfatal-warnings")
    // was: "a pure expression does nothing in statement position; you may be omitting necessary parentheses"
    tb.eval(tb.parse {
      """
        |  import scala.async.internal.AsyncId._
        |   async {
        |     if ("".isEmpty) {
        |       await(println("hello"))
        |       ()
        |     } else 42
        |   }
      """.stripMargin
    })
  }

  @Test
  // https://github.com/scala/async/issues/74
  def noDeadCodeWarningForAsyncThrow(): Unit = {
    val global = mkGlobal("-cp ${toolboxClasspath} -Yrangepos -Ywarn-dead-code -Xfatal-warnings -Ystop-after:refchecks")
    // was: "a pure expression does nothing in statement position; you may be omitting necessary parentheses"
    val source =
      """
        | class Test {
        |   import scala.async.Async._
        |   import scala.concurrent.ExecutionContext.Implicits.global
        |   async { throw new Error() }
        | }
      """.stripMargin
    val run = new global.Run
    val sourceFile = global.newSourceFile(source)
    run.compileSources(sourceFile :: Nil)
    assert(!global.reporter.hasErrors, global.reporter.asInstanceOf[StoreReporter].infos)
  }

  @Test
  def noDeadCodeWarningInMacroExpansion(): Unit = {
    val global = mkGlobal("-cp ${toolboxClasspath} -Yrangepos -Ywarn-dead-code -Xfatal-warnings -Ystop-after:refchecks")
    val source = """
        | class Test {
        |  def test = {
        |    import scala.async.Async._, scala.concurrent._, ExecutionContext.Implicits.global
        |    async {
        |      val opt = await(async(Option.empty[String => Future[Unit]]))
        |      opt match {
        |        case None =>
        |          throw new RuntimeException("case a")
        |        case Some(f) =>
        |          await(f("case b"))
        |      }
        |    }
        |  }
        |}
      """.stripMargin
    val run = new global.Run
    val sourceFile = global.newSourceFile(source)
    run.compileSources(sourceFile :: Nil)
    assert(!global.reporter.hasErrors, global.reporter.asInstanceOf[StoreReporter].infos)
  }

  @Test
  def ignoreNestedAwaitsInIDE_t1002561(): Unit = {
    // https://www.assembla.com/spaces/scala-ide/tickets/1002561
    val global = mkGlobal("-cp ${toolboxClasspath} -Yrangepos -Ystop-after:typer ")
    val source = """
        | class Test {
        |  def test = {
        |    import scala.async.Async._, scala.concurrent._, ExecutionContext.Implicits.global
        |    async {
        |      1 + await({def foo = (async(await(async(2)))); foo})
        |    }
        |  }
        |}
      """.stripMargin
    val run = new global.Run
    val sourceFile = global.newSourceFile(source)
    run.compileSources(sourceFile :: Nil)
    assert(!global.reporter.hasErrors, global.reporter.asInstanceOf[StoreReporter].infos)
  }
}
