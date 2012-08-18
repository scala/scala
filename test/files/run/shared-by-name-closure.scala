import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:uncurry -d " + testOutput.path

  override def code = """class T {
                        |  def foo(a: => Any) {}
                        |
                        |  val inlinable = 1
                        |  def notInlinable = 2
                        |  lazy val lazyNotInlinable = 3
                        |
                        |  foo("literal")
                        |  foo(inlinable)
                        |  foo(notInlinable)
                        |  foo(lazyNotInlinable)
                        |
                        |  () => ("literal")
                        |  () => (inlinable)
                        |  () => (notInlinable)
                        |  () => (lazyNotInlinable)
                        |
                        |  (x: Int) => ("literal")
                        |  (x: Int) => (inlinable)
                        |  (x: Int) => (notInlinable)
                        |  (x: Int) => (lazyNotInlinable)
                        |
                        |  (x: Int, y: String) => ("literal")
                        |  (x: Int, y: String) => (inlinable)
                        |  (x: Int, y: String) => (notInlinable)
                        |  (x: Int, y: String) => (lazyNotInlinable)
                        |
                        |  (x: Int, y: String, z: Any) => ("literal")
                        |  (x: Int, y: String, z: Any) => (inlinable)
                        |  (x: Int, y: String, z: Any) => (notInlinable)
                        |  (x: Int, y: String, z: Any) => (lazyNotInlinable)
                        |
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
