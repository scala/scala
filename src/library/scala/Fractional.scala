package scala

trait Fractional[T] extends Numeric[T] {
  def div(x: T, y: T): T
}
