import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:parser -Xprint-pos -d " + testOutput.path

  override def code = """class C
                        |
                        |
                        |
                        |// comment
                        |
                        |object O
                        |
                        |
                        |
                        |// comment
                        |trait T
                        |
                        |
                        |
                        |// comment
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
