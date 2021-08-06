package scala.lang.annotations

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.RunTesting

class RunTest extends RunTesting {
  import runner._

  @Test
  def annotationInfoNotErased(): Unit = {
    val code =
      """import scala.tools.testkit.Resource
        |import scala.annotation.meta.getter
        |class C {
        |  type Rg = Resource @getter
        |  @(Resource @getter)(`type` = classOf[Int]) def a = 0
        |  @Rg(`type` = classOf[Int])                 def b = 0
        |}
        |val c = classOf[C]
        |def typeArg(meth: String) = c.getDeclaredMethod(meth).getDeclaredAnnotation(classOf[Resource]).`type`
        |List("a", "b") map typeArg
        |""".stripMargin

    val i = Integer.TYPE
    assertEquals(run[List[Class[_]]](code), List(i, i))
  }
}
