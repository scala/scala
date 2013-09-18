trait Atomic[@specialized(Boolean) T] {
  def x: T

  def f(fn: T => T): Boolean = f(fn(x), true)
  def f[R](a: T, b: R): R = b
}
