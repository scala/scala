import scala.util.Random

package object tastytest {

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = ???
  }

  def getRandomNat: Int = ???

  def getRandomPos: Int = ???
}
