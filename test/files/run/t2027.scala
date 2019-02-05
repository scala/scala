object Test {
  def main(args: Array[String]): Unit = {
    def fibs: LazyList[Int] = LazyList.cons(0, LazyList.cons(1, fibs.zip(fibs.tail).map(p => p._1 + p._2)))
    println(fibs(2)) // stack overflow
  }
}
