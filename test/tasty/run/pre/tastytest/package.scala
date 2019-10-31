import scala.util.Random

package object tastytest {

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = t == u
  }

  def getRandomNat: Int = {
    val r = Random.nextInt
    val n = if (r == Int.MinValue) Int.MaxValue else r
    math.abs(n)
  }

  def getRandomPos: Int = {
    val potential = getRandomNat
    if (potential == 0) 1 else potential
  }
}
