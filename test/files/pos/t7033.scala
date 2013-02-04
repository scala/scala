import language.higherKinds
object Wrap {
  implicit class X[X](val a: X)

  X[Int](0)
}

class Wrap {
  implicit class Y[Y](val a: Y)
  Y[Int](0)
  implicit class Z[Z[_]](val a: Z[Wrap.this.Z[Z]])
  Z[List](List(new Z[List](null)))
}

case class X[X](val a: X)
