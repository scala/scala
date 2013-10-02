

trait Base[B] {
  def data: AnyRef;
}


class Suba[@specialized B](override val data: Array[B]) extends Base[B] {
  assert(data != null)
}


class Subopt[@specialized B](override val data: Option[B]) extends Base[B] {
  assert(data != null)
}


object Test {
  def main(args: Array[String]) {
    val ss = new Suba[String](Array(""))
    val si = new Suba[Int](Array(0))
    new Subopt[Int](Some(0))
  }
}
