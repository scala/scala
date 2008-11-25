package scalax

trait Integral[T] extends Numeric[T] {
  def quot(x: T, y: T): T
  def rem(x: T, y: T): T
}
