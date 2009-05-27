
// populate a priority queue a few different ways and make sure they all seem equal
object Test extends Application {
  import scala.collection.mutable.PriorityQueue
  import scala.util.Random.nextInt
  val pq1 = new PriorityQueue[String]
  val pq2 = new PriorityQueue[String]
  val pq3 = new PriorityQueue[String]
  val pq4 = new PriorityQueue[String]

  val strings = (1 to 20).toList map (i => List.fill((Math.abs(nextInt % 20)) + 1)("x").mkString)

  pq1 ++= strings
  pq2 ++= strings.reverse
  for (s <- strings) pq3 += s
  for (s <- strings.reverse) pq4 += s

  val pqs = List(pq1, pq2, pq3, pq4, pq1.clone, pq2.clone)

  for (queue1 <- pqs ; queue2 <- pqs) {
    assert(queue1 == queue2)
    assert(queue1.max == queue2.max)
  }
}
