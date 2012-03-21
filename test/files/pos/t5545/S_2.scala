trait F[@specialized(Int) T1, R] {
  def f(v1: T1): R
  def g = v1 => f(v1)
}
