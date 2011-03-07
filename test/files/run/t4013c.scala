class Suba[@specialized(Int) B](val data: Array[B]) {
  assert(data != null)
}


object Test {
  def main(args: Array[String]) {
    new Suba[Int](Array(0))
  }
}
