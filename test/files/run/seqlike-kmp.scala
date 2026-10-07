import collection.Seq
import collection.immutable.LazyList
import math.BigInt
import util.chaining._

object Test {
  val idxes = (-1 to 2) ++ (9 to 10)
  def str[A](xs: Seq[A]) = xs.mkString("(", ", ", ")")

  // the first index `>= from` such that...
  def first[A](source: Seq[A], tgt: Seq[A]) = {
    println(s"indexOfSlice of ${source.getClass}")
    for (from <- idxes) {
      val res = source.indexOfSlice(tgt, from = from)
      println(f"  ${str(tgt)} with idx >= $from%d = $res%d")
    }
  }
  // the last index `<= end` such that...
  def last[A](source: Seq[A], tgt: Seq[A]) = {
    println("lastIndexOfSlice")
    for (end <- idxes) {
      val res = source.lastIndexOfSlice(tgt, end)
      println(f"  ${str(tgt)} with idx <= $end%d = $res%d")
    }
  }

  def both(source: Seq[Int], idx: Int, len: Int) = {
    first(source, source.slice(idx, idx + len))
    last(source, source.slice(idx, idx + len))
  }

  def listed(source: Seq[Int], idx: Int, len: Int) = {
    first(source, source.slice(idx, idx + len).toList)
    last(source, source.slice(idx, idx + len).toList)
  }

  val fibs: LazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #::
      fibs.zip(fibs.tail).map{ n =>
        println(s"Adding ${n._1} and ${n._2}")
        n._1 + n._2
      }

  def main(args: Array[String]): Unit = {
    val source = (0 to 9).toVector
    both(source, 7, 1)
    both(source, 7, 2)
    both(source, 7, 3)
    both(source, 8, 2)
    both(source, 9, 1)
    val mut: Seq[Int] = source.toArray
    listed(mut, 7, 1)
    listed(mut, 7, 2)
    listed(mut, 7, 3)
    listed(mut, 8, 2)
    listed(mut, 9, 1)
    val xs: Seq[Int] = source.toList
    listed(xs, 7, 1)
    listed(xs, 7, 2)
    listed(xs, 7, 3)
    listed(xs, 8, 2)
    listed(xs, 9, 1)
    first(fibs.take(10), List(3,5))
    last(fibs, List(3,5))
  }
}
