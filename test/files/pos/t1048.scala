trait T[U] {
  def x: T[V] forSome { type V <: U }
}

object T {
  def unapply[U](t: T[U]): Option[T[V] forSome { type V <: U }] = Some(t.x)
}

object Test {
  def f[W](t: T[W]) = t match {
    case T(T(_)) => ()
  }
}

