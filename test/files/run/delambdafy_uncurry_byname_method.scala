import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:uncurry -Ydelambdafy:method -Ystop-after:uncurry -d " + testOutput.path

  override def code = """class Foo {
                        |  def bar(x: => String) = x
                        |  
                        |  def foo = bar("")
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
