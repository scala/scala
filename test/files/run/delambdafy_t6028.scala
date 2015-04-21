import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ydelambdafy:method -Xprint:lambdalift -d " + testOutput.path

  override def code = """class T(classParam: String) {
                        |  val field: String = ""
                        |  def foo(methodParam: String) = {val methodLocal = "" ; () => classParam + field + methodParam + methodLocal }
                        |  def bar(barParam: String) = { trait MethodLocalTrait { print(barParam) }; object MethodLocalObject extends MethodLocalTrait; MethodLocalObject }
                        |  def tryy(tryyParam: String) = { var tryyLocal = ""; () => try { tryyLocal = tryyParam } finally () }
                        |}
                        |""".stripMargin.trim

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
