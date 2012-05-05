class t1785 {
  def apply[T](x: Int) = 1
}

object test {
  (new t1785)[Int](1)
}
