import scala.collection.mutable.HashSet


object Test extends App {
  val h = new HashSet[Int]
  h += 1
  assert(!h.remove(0))
  assert(h(1))
  assert(h.remove(1))
  assert(!h(1))
  assert(!h.remove(1))
  assert(!h(1))
 }
