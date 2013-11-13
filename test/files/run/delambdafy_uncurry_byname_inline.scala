import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:uncurry -Ydelambdafy:inline -d " + testOutput.path

  override def code = """class Foo {
                        |  def bar(x: => Int) = x
                        |  
                        |  def foo = bar(1)
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
