import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{interactive, Settings}
import scala.tools.nsc.reporters.ConsoleReporter

object Test {

  def main(args: Array[String]) {
    val settings = new Settings
    settings.usejavacp.value = true
    val reporter = new ConsoleReporter(settings)

    val iglobal = new interactive.Global(settings, reporter)
    import iglobal._

    def getOrThrow[T](resp: Response[T]) = resp.get match {
      case Left(res) => res
      case Right(t) => throw t
    }

    def load(sourceFile: BatchSourceFile) = {
      val resp = new Response[Tree]
      askLoadedTyped(sourceFile, resp)
      getOrThrow(resp)
    }

    val prestestSrc = new BatchSourceFile("Prestest.scala",
      """
        |package prestest
        |
        |object Prestest {
        |  trait Root {
        |    def meth = 5
        |  }
        |}
        |
      """.stripMargin
    )

    load(prestestSrc)

    val opsSrc = new BatchSourceFile("ops.scala",
      """
        |package com.whatever
        |
        |//import prestest.Prestest.Root // this was okay
        |
        |object Utils {
        |
        |  import prestest.Prestest.Root // but this import was not recognised when typecking the implicit class parameter formal type
        |
        |  implicit class rootOps(root: Root) {
        |    def implicitMethod: Int = 42
        |  }
        |
        |}
      """.stripMargin)

    load(opsSrc)

    if(reporter.hasErrors) {
      throw new Exception("There were errors")
    }
  }

}
