import scala.collection.{ mutable, immutable, generic }
import immutable.ListSet

object Test {    
  def main(args: Array[String]): Unit = {
    val xs = ListSet(-100000 to 100001: _*)
   
    assert(xs.size == 200002)
    assert(xs.sum == 100001)
    
    val ys = ListSet[Int]()
    val ys1 = (1 to 12).grouped(3).foldLeft(ys)(_ ++ _)
    val ys2 = (1 to 12).foldLeft(ys)(_ + _)
    
    assert(ys1 == ys2)
  }
}


