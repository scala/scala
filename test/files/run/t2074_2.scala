import scala.collection.immutable.Vector
import scala.collection.generic.VectorView

object Test {
  val v = new VectorView[Int, Vector[Int]] {
    def underlying = Vector(1,2,3)
    def apply(idx: Int) = underlying(idx)
    def length = underlying.length
  }

  def main(args: Array[String]): Unit = {
    println(go)
  }
  def go = v zip v
}
