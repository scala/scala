object Test {
  def coll = new Traversable[String] {
    override def foreach[U](f:String=>U) { f("1") }
  }
  val dropped = coll.view drop 1

  def main(args: Array[String]): Unit = {
    println(dropped.isEmpty)
    println(dropped.force.isEmpty)
  }
}
