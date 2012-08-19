import scala.collection.immutable._

object Test extends App {
  val set = HashSet((0 until 100):_*)
  for(i<-0 until 100) {
    val filtered = set.filter(_ < i)
    require(filtered.forall(_ < i))
    require(filtered.size == i)
  }
}
