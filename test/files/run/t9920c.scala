class C0
trait T { self: C0 =>
  def test = {
    object Local

    class C2 {
      class C1 {
        Local
      }
      T.this.toString
      new C1
    }
    new C2()
  }
}

object Test extends C0 with T {
  def main(args: Array[String]): Unit = {
    test
  }
}
