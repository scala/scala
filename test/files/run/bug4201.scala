object Test {
  def main(args: Array[String]): Unit = {
    val f = 0.0 to 1.0 by 1.0 / 3.0
    assert(f.size == 4)
  }
}

