import scala.collection.immutable._

object Test extends App {
  val res0 = TreeSet(1, 2, 3, 4, 5, 6)
  val res1 = res0.map(x => x)
  println(res0.toList == res1.toList)
  println(res1.getClass)
}
