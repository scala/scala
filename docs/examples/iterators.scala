package examples;

object iterators {

  def Array(elems: Double*): Array[Double] = {
    val ar = new Array[Double](elems.length)
    for (val i <- Iterator.range(0, elems.length))
      ar(i) = elems(i)
    ar
  }

  def printArray(xs: Array[Double]) =
    Iterator.fromArray(xs) foreach { x => Console.println(x) }

  def findGreater(xs: Array[Double], limit: Double) =
    Iterator.fromArray(xs)
      .zip(Iterator.from(0))
      .filter{case Pair(x, i) => x > limit }
      .map{case Pair(x, i) => i}

  def main(args: Array[String]): Unit = {
    val ar = Array/*[Double]*/(6, 2, 8, 5, 1)
    printArray(ar)
    Console.println("Elements greater than 3.0:")
    findGreater(ar, 3.0) foreach { x => Console.println(ar(x)) }
  }

}
