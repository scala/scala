class J3 {
  def f[K, K1 >: K, V](x: Map[K1, V]): Map[K, V] = sys.error("")
}

object Test {
  (new J3).f(Map[Int, Int]())
}
