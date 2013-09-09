import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Yrangepos -Xprint:patmat -Xprint-pos -d " + testOutput.path

  override def code = """
abstract class A[T] {
  val foo: Set[_ <: T] = null
  val bar: Set[_ <: T]
}""".trim

  override def show(): Unit = Console.withErr(System.out)(compile())
}
