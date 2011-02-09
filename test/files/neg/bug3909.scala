object DO {
  class Extras { }
  object Extras { val defaultValue = new Extras }

  def m1(str: String, extraStuff: Extras = Extras.defaultValue): Int = str.length
  def m1(i: Int, extraStuff: Extras = Extras.defaultValue): Int = 2 * i

  def main(args: Array[String]): Unit = {
    val m1s = m1("foo")
    val m1i = m1(42)
  }
}
