import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:uncurry -Ydelambdafy:inline"

  override def code = """class Foo {
                        |  def bar(x: => Int) = x
                        |  
                        |  def foo = bar(1)
                        |}
                        |""".stripMargin.trim

  override def show(): Unit =
    Console.withErr(System.out) {
      compile()
    }
}
