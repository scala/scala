package scala

/**
 * @since 2.8
 */
trait Integral[T] extends Numeric[T] {
  def quot(x: T, y: T): T
  def rem(x: T, y: T): T

  class IntegralOps(lhs: T) extends Ops(lhs) {
    def /(rhs: T) = quot(lhs, rhs)
    def %(rhs: T) = rem(lhs, rhs)
  }
  override implicit def mkNumericOps(lhs: T): IntegralOps = new IntegralOps(lhs)
}
