import collection.mutable._

object Test {
  def main(args: Array[String]): Unit = {
    val b = ListBuffer(1, 2, 3)
    b ++= b
  }
}
