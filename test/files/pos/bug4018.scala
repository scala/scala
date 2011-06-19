trait M[V[_]]

class Cls[V[_]](c: M[V])

object Cls{
  def apply[V[_]](c: M[V]): Cls[V] = new Cls[V](c)
}

object test {
  val c: M[Option] = new M[Option] {}
  new Cls(c)         // does not infer.
  new Cls[Option](c) // okay
  Cls(c)             // okay
}

