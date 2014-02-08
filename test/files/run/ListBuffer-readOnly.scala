object Test {
  def main(args: Array[String]) {
    val buf = scala.collection.mutable.ListBuffer[Int](1, 2)
    def f(): List[Int] = buf.readOnly.toList

    val innocent = f()
    println(innocent)

    buf ++= (1 to 100)
    println(innocent)
  }
}
