abstract class List[+a] {
  def head: a
  def tail: List[a]
  def cons[b >: a](x: b): List[b] = new Cons[b, a](x, this)
}

object Nil extends List[Nothing] {
  def error(msg: String): Nothing = throw new java.lang.Error(msg)
  def head: Nothing = error("Nil.head")
  def tail: List[Nothing] = error("Nil.tail")
}

class Cons[c, d <: c](x: c, xs: List[d]) extends List[c] {
  def head: c = x
  def tail: List[c] = xs
}
