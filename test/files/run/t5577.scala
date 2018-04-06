import collection._

object Test {
  class AlarmingBuilder[T] extends mutable.ListBuffer[T] {
    override def sizeHint(x: Int) {
      println("Received a size hint: " + x)
      super.sizeHint(x)
    }
  }

  def main(args: Array[String]) {
    val iteratorBuilder = (new AlarmingBuilder[Int]) mapResult {
      res => res.iterator
    }

    iteratorBuilder.sizeHint(10)
    iteratorBuilder ++= (0 until 10)
    iteratorBuilder.result.foreach(println)
  }

}
