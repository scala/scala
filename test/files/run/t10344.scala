import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Vprint:typer -language:higherKinds -Ystop-after:typer"

  override def code = """
object t10344 {
  def unwrap[F[_]](f: F[Unit] => Unit): Unit = ()
  val f: (=> Unit) => Unit = { _ => () }
  unwrap(f)
}
  """

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
