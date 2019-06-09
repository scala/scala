import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Vprint:patmat -Vprint-pos -d " + testOutput.path

  override def code =
    """
      |object Case3 {
      |  def unapply(z: Any): Option[Int] = Some(-1)
      |
      |  "" match {
      |    case Case3(nr) => ()
      |  }
      |}
      |object Case4 {
      |  def unapplySeq(z: Any): Option[List[Int]] = None
      |
      |  "" match {
      |    case Case4(nr) => ()
      |  }
      |}
      |object Case5 {
      |  def unapply(z: Any): Boolean = true
      |
      |  "" match {
      |    case Case4() => ()
      |  }
      |}
      |object Case6 {
      |  def unapply(z: Int): Option[Int] = Some(-1)
      |
      |  0 match {
      |    case Case6(nr) => ()
      |  }
      |}
      |""".stripMargin.trim

  override def show(): Unit = {
    // Now: [84][84]Case3.unapply([84]x1);
    // Was: [84][84]Case3.unapply([64]x1);
    Console.withErr(System.out) {
      compile()
    }
  }
}
