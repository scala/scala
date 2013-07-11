
import scala.language.{ existentials }
trait Sequ[A] {
  def toArray: Array[T forSome {type T <: A}]
}

class RichStr extends Sequ[Char] {
  // override to a primitive array
  def toArray: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
}

object Test extends App {
  val x: RichStr = new RichStr

  println((x: Sequ[Char]).toArray.deep) // calling through the bridge misses unboxing
}
