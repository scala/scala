trait Base[+A] {
  def get: A = null.asInstanceOf[A]
}

trait SubBase extends Base[Any] {
  override def get: Any = ""
}

class Hi extends Base[Int] with SubBase

object Test {
  def main(args: Array[String]): Unit = {
    val hi = new Hi
    val base: Base[Int] = hi
    val y: Int = base.get // ClassCastException: java.lang.String cannot be cast to java.lang.Integer
    println(y)
  }
}
