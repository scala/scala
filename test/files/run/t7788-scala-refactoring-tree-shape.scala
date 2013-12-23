import scala.tools.nsc.util
import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:typer -d " + testOutput.path

  override def code = """
                        | package a.b.c
                        | import language._
                        | object TestImplicits {
                        |   implicit def stringToBytes(s: String): Array[Byte] = s.getBytes
                        | }
                        | import TestImplicits._
                        |
                        | object Tester {
                        |   "":Array[Byte] // show
                        |   TestImplicits
                        | }
                        |""".stripMargin.trim

  override def show(): Unit = {
    val out = util.stringFromStream {stream =>
      Console.withOut(stream) {
        compile()
      }
    }
    // Scala refactoring tests expect the implicit view to be `TestImplicits.stringToBytes`,
    // and not the long-form `c.TestImplicits.stringToBytes`.
    println(out.lines.filter(_.contains(".stringToBytes")).toList.mkString("\n"))
  }
}
