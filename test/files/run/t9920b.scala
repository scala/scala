class C0
trait T {
  def test = {
    object Local

    class C1 {
      Local
    }
    new C1()
  }
}

object Test extends C0 with T {
  def main(args: Array[String]): Unit = {
    test
  }
}
