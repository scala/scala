import scala.collection.immutable.Vector
import scala.collection.VectorView

object Test {
  val v = new VectorView[Int, Vector[Int]] {
    def underlying = Vector(1,2,3)
    def apply(idx: Int) = underlying(idx)
    def length = underlying.length
  }
  val w = Vector(1, 2, 3).view

  def main(args: Array[String]): Unit = {
    println(v)
    println(w)
    println(go)
  }
  def go = v zip v
}
