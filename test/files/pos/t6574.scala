class Bad[X, Y](val v: Int) extends AnyVal {
  def vv = v
  @annotation.tailrec final def foo[Z](a: Int)(b: String) {
    this.foo[Z](a)(b)
  }

  @annotation.tailrec final def differentReceiver {
    {(); new Bad[X, Y](0)}.differentReceiver
  }

  @annotation.tailrec final def dependent[Z](a: Int)(b: String): b.type = {
    this.dependent[Z](a)(b)
  }
}

class HK[M[_]](val v: Int) extends AnyVal {
  def hk[N[_]]: Unit = if (false) hk[M] else ()
}

