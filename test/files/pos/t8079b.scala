trait F1[/* - */T, /* + */ R]

object Test {
  import scala.annotation.unchecked._
  type VariantF1[-T, +R] = F1[T @uncheckedVariance, R @uncheckedVariance]
  trait C[+T] { def foo: VariantF1[Any, T] }
}
