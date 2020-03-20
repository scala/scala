object Test {
  class C[K,V]
  class J { def f[K,V](x: C[_ >: K, _ <: V]): C[K,V] = null }
  object o { def go() = (new J).f(new C) }

  class D[K,V]
  def f[K,V](x: D[_ >: K, _ <: V]) = x
  f(new D)
}
