object Test {
  def main(args: Array[String]) {
    def fibs: Stream[Int] = Stream.cons(0, Stream.cons(1, fibs.zip(fibs.tail).map(p => p._1 + p._2)))
    println(fibs(2)) // stack overflow
  }
}
