



object Test {
  def main(args: Array[String]) {
    import collection.mutable._
    val pq = PriorityQueue[Int]()
    pq ++= List(1, 2, 3, 4)
    
    pq.orderedIterator.foreach(println)
  }
}
