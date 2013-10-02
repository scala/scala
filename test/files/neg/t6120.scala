class A {
  implicit class BooleanOps(val b: Boolean) {
    @deprecated("bobo", "2.11.0") def bippy() = 5
  }
  def f = null == null bippy
  def g = true.bippy
}
