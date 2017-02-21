object Test {
  def main(args: Array[String]): Unit = {
    assert(1.##.## == 1) // was java.lang.NoSuchMethodError: java.lang.Object.$hash$hash()I
  }
}
