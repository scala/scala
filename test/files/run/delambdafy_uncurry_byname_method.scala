import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:uncurry -Ydelambdafy:method -Ystop-after:uncurry"

  override def code = """class Foo {
                        |  def bar(x: => String) = x
                        |  
                        |  def foo = bar("")
                        |}
                        |""".stripMargin.trim

  override def show(): Unit =
    Console.withErr(System.out) {
      compile()
    }
}
