import scala.collection.{ mutable, immutable, generic }
import immutable.ListSet

object Test {
  def main(args: Array[String]): Unit = {
    val xs = ListSet(-100000 to 100001: _*)

    assert(xs.size == 200002)
    assert(xs.sum == 100001)
  }
}


