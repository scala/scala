trait A1 {
  def f[T[+_]] = ()
}

trait B1 extends A1 {
  override def f[T[_]] = ()
}


trait A2 {
  def f[T[-_]] = ()
}

trait B2 extends A2 {
  override def f[T[_]] = ()
}


trait A3 {
  def f[T[X[_]]] = ()
}

trait B3 extends A3 {
  override def f[T[X[+_]]] = ()
}
