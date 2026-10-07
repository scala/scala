import scala.annotation.unchecked.uncheckedOverride

trait Hordering[T] extends java.util.Comparator[T] {
  // overriding on java 26 and later, not overriding before
  @uncheckedOverride def max[U <: T](x: U, y: U): U = x
  @uncheckedOverride def min[U <: T](x: U, y: U): U = x 
}

trait Base[T] {
  def max[U <: T](x: U, y: U): U = x

  def mux(x: String = "ex") = x
}

trait Sub[T] extends Base[T] {
  @uncheckedOverride def max[U <: T](x: U, y: U): U = x // overriding ok
  @uncheckedOverride def min[U <: T](x: U, y: U): U = x // not overriding ok

  // overriding default
  @uncheckedOverride def mux(x: String = "nox") = x

  def test = mux()
}
