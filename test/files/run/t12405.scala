import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Vprint:patmat -Ystop-after:patmat"

  override val code =
    """final class C[A](val x: A) extends AnyVal {
      |  def isEmpty: Boolean = ???
      |  def get: A = ???
      |}
      |object C {
      |  def unapply[T](c: C[T]): C[T] = c
      |}
      |class Test {
      |  def m1(a: Any) = a match {
      |    case C(x) => x
      |    case _ => null
      |  }
      |
      |  def m2(c: C[String]) = c match {
      |    case C(x) => x
      |    case _ => ""
      |  }
      |}
      |""".stripMargin

  override def show(): Unit = Console.withErr(System.out) {
    compile()
  }
}
