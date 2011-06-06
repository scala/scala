package bippy

object Test {
  def main(args: Array[String]): Unit = {
    J.f()
    S1.f1()
    val x = new S2
    x.f2()
  }
}
