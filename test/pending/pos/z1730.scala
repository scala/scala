// /scala/trac/z1730/a.scala
// Wed May 23 07:41:25 PDT 2012

class X[R] {
  def xx(value: => R, addTweak: Boolean = true) = 0
}

class Boo {
  implicit def toX[R](v: R) : X[R] = null
  def goo2 {
    3.xx(34)
  }
}
