import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:specialize -d " + testOutput.path

  override def code = "class Foo { val f = (param: Int) => param } "

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
