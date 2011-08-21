object Test {
  def f() = {
    val ar = Array.ofDim[Int](5)
    var x = 0

    while (x<=5) {
      println(x)
      val a = ar(x)
      x+=1
    }
  }
  def main(args: Array[String]): Unit = {
    try   { f() ; assert(false, "should have thrown exception") }
    catch { case _: ArrayIndexOutOfBoundsException => () }
  }
}
