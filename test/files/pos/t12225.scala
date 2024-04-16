//> using options -Ydebug
object Test {
  def foo(arr: Array[Int]): Unit = {
    val Array(x, y) = arr
  }
}
