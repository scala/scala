import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.mutable

object Test extends App {

  def testMap(): Unit = {
    val mapJ = new java.util.HashMap[Int, String]
    val mapS: mutable.Map[Int, String] = mapJ

    (10 to 20).foreach(i => mapS += ((i, i.toString)))
    assert(11 == mapS.size)

    // ConcurrentModificationException thrown in the following line
    mapS.retain((i, str) => i % 2 == 0)
    assert(6 == mapS.size)
  }

  def testSet(): Unit = {
    val mapJ = new java.util.HashSet[Int]
    val mapS: mutable.Set[Int] = mapJ

    (10 to 20).foreach(i => mapS += i)
    assert(11 == mapS.size)

    // ConcurrentModificationException thrown in the following line
    mapS.retain((i) => i % 2 == 0)
    assert(6 == mapS.size)
  }

  testSet()
  testMap()
}
