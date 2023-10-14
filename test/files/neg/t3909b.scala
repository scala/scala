object DO {

  def m1(str: String, extraStuff: String = "stuff"): Int = ???
  def m1(i: Int, extraStuff: Int = "42".toInt): Int = ???

  def main(args: Array[String]): Unit = {
    val m1s = m1("foo")
    val m1i = m1(42)
  }
}
