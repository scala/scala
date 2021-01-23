import scala.tools.partest.SeparateCompileTest

import scala.tools.nsc.Global

object Test extends SeparateCompileTest {
  val source0 =
    """package a
      |protected[a] class A
      |""".stripMargin
  val source1 =
    """package example
      |import a.A
      |object Test { val x = new a.A }
      |""".stripMargin

  override def sourcess: List[List[String]] =
    List(List(source0), List(source1))

  // ok to fail compilation
  override def check(global: Global)(
      source: String,
      result: Either[Unit, global.CompilationUnit]): Unit = ()
}
