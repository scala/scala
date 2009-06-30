object Test {
  def main(args: Array[String]) {
    println(Nil.getClass.getGenericSuperclass)
    println(None.getClass.getGenericSuperclass)
  }
}
