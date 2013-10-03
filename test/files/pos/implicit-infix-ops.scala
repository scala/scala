object Test {
  import Ordering.Implicits._
  import Numeric.Implicits._

  def f1[T: Numeric](x: T, y: T, z: T)  = x + y + z
  def f2[T: Ordering](x: T, y: T, z: T) = if (x < y) (z > y) else (x < z)
}

object Int {
  import Ordering.Implicits._
  import math.Integral.Implicits._

  def f1[T: Integral](x: T, y: T, z: T)  = (x + y + z) / z
  def f2[T: Ordering](x: T, y: T, z: T) = if (x < y) (z > y) else (x < z)
}

object Frac {
  import Ordering.Implicits._
  import math.Fractional.Implicits._

  def f1[T: Fractional](x: T, y: T, z: T)  = (x + y + z) / z
  def f2[T: Ordering](x: T, y: T, z: T) = if (x < y) (z > y) else (x < z)
}