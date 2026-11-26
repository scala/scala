// The bound of `a` mentions `b`, so instantiating `a` computes the base types of `b`
// before `b` is narrowed to `Cov[String]`.
trait Cov[+A] { def head: A }
trait P[+A <: B, +B] { def b: B }

class Test {
  def t1(p: P[Cov[String], Cov[String]]) = p match {
    case l: P[a, b] => val x: b = l.b; x.head: String
  }
  def t2(p: P[Cov[String], Cov[String]]) = p match {
    case l: P[a, b] => l.b.head: String
  }
}
