import collection.mutable._

object Test {
  def main(args: Array[String]): Unit = {
    val b = new ListBuffer[Int]
    b += 1
    b ++= b
  }
}
