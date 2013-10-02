
import scala.language.{ existentials }

object Test extends App {
  {
    val (left, right) = Seq((1, "a"), (1, "a"), (1, "a"), (3, "c")).view.unzip
    println(left.isInstanceOf[scala.collection.SeqViewLike[_,_,_]])
    val (l, m, r) = Seq((1, 1.0, "a"), (1, 1.0, "a"), (1, 1.0, "a"), (3, 3.0, "c")).view.unzip3
    println(l.isInstanceOf[scala.collection.SeqViewLike[_,_,_]])
  }
  {
    val (left, right) = Iterable((1, "a"), (1, "a"), (1, "a"), (3, "c")).view.unzip
    println(left.isInstanceOf[scala.collection.IterableViewLike[_,_,_]])
    val (l, m, r) = Iterable((1, 1.0, "a"), (1, 1.0, "a"), (1, 1.0, "a"), (3, 3.0, "c")).view.unzip3
    println(l.isInstanceOf[scala.collection.IterableViewLike[_,_,_]])
  }
  {
    val (left, right) = Traversable((1, "a"), (1, "a"), (1, "a"), (3, "c")).view.unzip
    println(left.isInstanceOf[scala.collection.TraversableViewLike[_,_,_]])
    val (l, m, r) = Traversable((1, 1.0, "a"), (1, 1.0, "a"), (1, 1.0, "a"), (3, 3.0, "c")).view.unzip3
    println(l.isInstanceOf[scala.collection.TraversableViewLike[_,_,_]])
  }
}
