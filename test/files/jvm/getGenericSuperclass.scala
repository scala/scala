object Test {
  def main(args: Array[String]): Unit = {
    println(Nil.getClass.getGenericSuperclass)
    println(None.getClass.getGenericSuperclass)
  }
}
