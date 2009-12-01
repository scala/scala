object Test
{
  val f1 = -0.0
  val f2 = -(0.0)
  def main(args: Array[String]): Unit = {
    assert(f1.toString startsWith "-")
    assert(f2.toString startsWith "-")
  }
}
