object Test extends App {
  class Test(n: Int) extends Iterable[Int] {
    private var i = 0
    def iterator = new Iterator[Int] {
      def hasNext = i < n
      def next =
        if (hasNext) { val v = i; i += 1; v }
        else throw new IndexOutOfBoundsException("empty iterator")
    }
  }
  {
    val x = new Test(10)
    println(x.isEmpty)
    println(x.mkString(","))
  }
  {
    val x = new Test(10)
    println(x.filter(_ > 4).mkString(","))
  }
  {
    val x = new Test(10)
    val y = x.partition(_ % 2 == 0)
    println(y._1.mkString(","))
    println(y._2.mkString(","))
  }
}
