import collection.mutable.ArrayBuffer

object Test {
  val x = (1 to 10).to(ArrayBuffer)

  def main(args: Array[String]): Unit = {
    x mapInPlace (_ * 2)
    assert(x.sum == (1 to 10).sum * 2)
  }
}
