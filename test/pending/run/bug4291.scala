import scala.tools.partest._

object Test extends SigTest {
  def main(args: Array[String]): Unit = {
    show[List[_]]("apply")
    show[Option[_]]("get")
    show[Function1[_, _]]("apply")
    show[Traversable[_]]("flatMap")
  }
}
