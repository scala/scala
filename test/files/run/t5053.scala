import scala.language.{ existentials }
import collection.View

object Test extends App {
  {
    val (left, right) = Seq((1, "a"), (1, "a"), (1, "a"), (3, "c")).view.unzip
    println(left.isInstanceOf[View[_]])
    val (l, m, r) = Seq((1, 1.0, "a"), (1, 1.0, "a"), (1, 1.0, "a"), (3, 3.0, "c")).view.unzip3
    println(l.isInstanceOf[View[_]])
  }
  {
    val (left, right) = Iterable((1, "a"), (1, "a"), (1, "a"), (3, "c")).view.unzip
    println(left.isInstanceOf[View[_]])
    val (l, m, r) = Iterable((1, 1.0, "a"), (1, 1.0, "a"), (1, 1.0, "a"), (3, 3.0, "c")).view.unzip3
    println(l.isInstanceOf[View[_]])
  }
}
