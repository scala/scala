module iterators {

  def printArray(xs: Array[Int]) =
    Iterator.fromArray(xs) foreach (x => System.out.println(x));

  def findGreater(xs: Array[Double], limit: Double) =
    Iterator.fromArray(xs)
      .zip(Iterator.from(0))
      .filter{case Pair(x, i) => x > limit}
      .map{case Pair(x, i) => i}

}
