import scala.collection.immutable._

object Test extends Application {
  val res0 = TreeSet(1, 2, 3)
  val res1 = res0.map(x => x)
  println(res1.getClass)
}
