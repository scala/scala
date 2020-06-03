import scala.tools.partest._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:patmat -Vprint-types"

  override def code = """class C {
    def foo[A](a: A, b: A with C, i: Int) = i match {
      case 0 => a
      case 1 => b
    }
  }
  """

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
