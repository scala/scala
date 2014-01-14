class Optiony[X] { def isEmpty = true; def get: X = ??? }
class Seqy[X] { def head: X = ???; def length = 0; def apply(i: Int): X = ??? }

object G {
  def unapply(m: Any): Optiony[_] = ???
}

object H {
  def unapplySeq(m: Any): Optiony[Seqy[_]] = ???
}

object Test {
  (0: Any) match {
    case G(v) => v
    case H(v) => v
    case _ =>
  }
}
