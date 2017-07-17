object Test extends test.ScalaBipp {
  override def f(): Int = super.f()

  def main(args: Array[String]): Unit = {
    f()
  }
}
