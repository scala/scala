object Test {
  class C
  class D

  def main(args: Array[String]): Unit = {
    val z: Class[C] = classOf
    val z2: Class[D] = classOf[D]

    println(z)
    println(z2)
  }
}
