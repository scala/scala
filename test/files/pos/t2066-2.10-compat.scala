import language._
trait A1 {
  def f[T[_]] = ()
}

trait B1 extends A1 {
  override def f[T[+_]] = ()
}

trait C1 extends A1 {
  override def f[T[-_]] = ()
}


trait A2 {
  def f[T[+_]] = ()
}

trait B2 extends A2 {
  override def f[T[_]] = () // okay
}

trait C2 extends A2 {
  override def f[T[-_]] = ()
}


trait A3 {
  def f[T[-_]] = ()
}

trait B3 extends A3 {
  override def f[T[_]] = () // okay
}

trait C3 extends A3 {
  override def f[T[-_]] = ()
}


trait A4 {
  def f[T[X[+_]]] = ()
}

trait B4 extends A4 {
  override def f[T[X[_]]] = ()
}

trait A5 {
  def f[T[X[-_]]] = ()
}

trait B5 extends A5 {
  override def f[T[X[_]]] = ()
}



trait A6 {
  def f[T[X[_]]] = ()
}

trait B6 extends A6 {
  override def f[T[X[+_]]] = () // okay
}
trait C6 extends A6 {
  override def f[T[X[_]]] = () // okay
}
trait D6 extends A6 {
  override def f[T[X[-_]]] = ()
}
