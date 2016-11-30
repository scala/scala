class Bad[X, Y](val v: Int) extends AnyVal {
  def vv = v
  @annotation.tailrec final def foo[Z](a: Int)(b: String) {
    this.foo[Z](a)(b)
  }

  @annotation.tailrec final def differentReceiver {
    {(); new Bad[X, Y](0)}.differentReceiver
  }

  // The original test case fails with the new is/asInstanceOf semantics
  // introduced along with -Yliteral-types because the method has a
  // singleton result type which cannot be erased correctly.
  // See: neg/sip23-tailrec-singleton.scala
  //@annotation.tailrec final def dependent[Z](a: Int)(b: String): b.type = {
  //  this.dependent[Z](a)(b)
  //}

  // Replacement test case
  @annotation.tailrec final def dependent[Z](a: Int)(b: String): Option[b.type] = {
    this.dependent[Z](a)(b)
  }
}

class HK[M[_]](val v: Int) extends AnyVal {
  def hk[N[_]]: Unit = if (false) hk[M] else ()
}

