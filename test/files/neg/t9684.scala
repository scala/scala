
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer

trait Test {
  null.asInstanceOf[java.util.List[Int]] : Buffer[Int]

  null.asInstanceOf[Iterable[Int]] : java.util.Collection[Int]
}
