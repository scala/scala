object Test {
  import Ordering.Implicits._
  import Numeric.Implicits._

  def f1[T: Numeric](x: T, y: T, z: T)  = x + y + z
  def f2[T: Ordering](x: T, y: T, z: T) = if (x < y) (z > y) else (x < z)
}
