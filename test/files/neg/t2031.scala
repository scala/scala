import scala.collection.immutable._

object Test extends App {
 val res0 = TreeSet(1, 2, 3)

 //res0.map(x => x)(TreeSet.newBuilder[Int])

 res0.map(x => x)(TreeSet.newBuilder)
}
