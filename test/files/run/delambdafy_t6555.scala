import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:specialize -Ydelambdafy:method"

  override def code = "class Foo { val f = (param: String) => param } "

  override def show(): Unit =
    Console.withErr(System.out) {
      compile()
    }
}
