import collection.immutable.SortedMap
import scala.math.Ordering.Implicits._

object Test {
  implicit val ord: Ordering[Array[Byte]] = Ordering.by(x => x.toIterable: collection.Seq[Byte])

  def main(args: Array[String]): Unit = {
    val m = SortedMap(Array[Byte](1) -> 0)
    println(m.rangeTo(Array[Byte](1)).toSeq flatMap (_._1.mkString))
    println(m.rangeFrom(Array[Byte](1)).toSeq flatMap (_._1.mkString))
  }
}
