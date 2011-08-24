class C[T](x : T = null)

object Test {
  def main(args: Array[String]): Unit = {
    new C[Int]
  }
}
