package tastytest

object TestInner {

  def test1 = assert(Inner.Foo.Bar != null)
  def test2 = assert(new Inner.Foo(){}.isInstanceOf[Inner.Foo])

  def main(args: Array[String]): Unit = {
    test1
    test2
    println("Suite passed!")
  }
}