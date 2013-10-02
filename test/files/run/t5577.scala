


import collection._



object Test {

  class AlarmingBuffer[T] extends mutable.ArrayBuffer[T] {
    override def sizeHint(x: Int) {
      println("Received a size hint: " + x)
      super.sizeHint(x)
    }
  }

  def main(args: Array[String]) {
    val iteratorBuilder = (new AlarmingBuffer[Int]) mapResult {
      res => res.iterator
    }

    iteratorBuilder.sizeHint(10)
    iteratorBuilder ++= (0 until 10)
    iteratorBuilder.result.foreach(println)
  }

}
