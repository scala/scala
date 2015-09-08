sealed trait Base
case object O1 extends Base
case object O2 extends Base {
  def foo: Int = 0
}

sealed trait Base2
case object O3 extends Base2

case object O4 extends Base with Base2

object Test {
  val a /*: Product with Serializable with Base */ = if (true) O1 else O2
  a match {
    case null =>
  }

  def t1(a: Product with Base with Base2) = a match {
    case null => // O1..O3 should *not* be possible here
  }

  def t2(a: Product with Base { def foo: Int }) = a match {
    case null => // O2 in the domain
  }

  def t3(a: Product with Base { def bar: Int }) = a match {
    case null => // nothing in the domain
  }
}
