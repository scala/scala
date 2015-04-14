object Test {
  def main(args: Array[String]): Unit = {
    val f = (x: Int) => -x
    println(f.getClass.getSuperclass.getName)
  }
}
