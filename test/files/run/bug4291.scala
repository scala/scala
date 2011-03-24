import scala.tools.partest._

object Test extends SigTest {
  def main(args: Array[String]): Unit = {
    show[List[_]]("apply")
    show[Option[_]]("get")
    show[Function1[_, _]]("apply")

    for (name <- List("map", "flatMap", "filter", "head", "groupBy")) {
      show[Traversable[_]](name)
      show[Iterable[_]](name)
      show[Seq[_]](name)
      show[Set[_]](name)
      show[Map[_,_]](name)
      show[Vector[_]](name)
      show[Range](name)
    }
  }
}
