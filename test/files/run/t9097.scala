import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ydelambdafy:method -Xprint:delambdafy -d " + testOutput.path

  override def code = """package o
                        |package a {
                        |  class C {
                        |    def hihi = List(1,2).map(_ * 2)
                        |  }
                        |}
                        |package object a {
                        |  def f = 1
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    val baos = new java.io.ByteArrayOutputStream()
    Console.withOut(baos)(Console.withErr(baos)(compile()))
    val out = baos.toString("UTF-8")
    // was 2 before the fix, the two PackageDefs for a would both contain the ClassDef for the closure
    assert(out.lines.count(_ contains "class hihi$1") == 1, out)
  }
}
