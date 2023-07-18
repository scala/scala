
import scala.jdk.CollectionConverters._

// superclass can't write other erased types so as to clash with bridge
class B[A: Ordering] extends Comparer with Comparable[java.util.List[A]] {
  def compareTo(other: java.util.List[A]): Int = 0
}
object C {
  def main(args: Array[String]): Unit = println {
    new B[Int]().compareTo(List(42).asJava)
  }
}
