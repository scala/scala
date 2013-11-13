import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ydelambdafy:inline -Xprint:lambdalift -d " + testOutput.path

  override def code = """class T(classParam: Int) {
                        |  val field: Int = 0
                        |  def foo(methodParam: Int) = {val methodLocal = 0 ; () => classParam + field + methodParam + methodLocal }
                        |  def bar(barParam: Int) = { trait MethodLocalTrait { print(barParam) }; object MethodLocalObject extends MethodLocalTrait; MethodLocalObject }
                        |  def tryy(tryyParam: Int) = { var tryyLocal = 0; () => try { tryyLocal = tryyParam } finally () }
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
