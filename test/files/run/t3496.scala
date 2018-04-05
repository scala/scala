// ticket #3496
object Test {

  def main(args: Array[String]) {
    val s = LazyList.from(1)
    s.take(5)
    s.drop(5)
    s.splitAt(5)
  }

}
