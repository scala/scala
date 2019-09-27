package tastytest

object TestInner {

  def test1 = assert(Inner.Foo != null)

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}