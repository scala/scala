object Test extends Application {
  class Test(n: Int) extends Iterable[Int] {
    private var i = 0
    def elements = new Iterator[Int] {
      def hasNext = i < n
      def next =
        if (hasNext) { val v = i; i += 1; v }
        else throw new IndexOutOfBoundsException("empty iterator")
    }
  }
  val x = new Test(10)
  println(x.isEmpty)
  println(x.mkString(","))
}
