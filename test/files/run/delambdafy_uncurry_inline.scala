import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:uncurry -Ydelambdafy:inline -d " + testOutput.path

  override def code = """class Foo {
                        |  def bar = {
                        |    val f = {x: Int => x + 1}
                        |  }
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
