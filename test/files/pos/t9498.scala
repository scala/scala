trait Inv[A] { def head: A }
trait Cov[+A] { def head: A }

class Test {
  def inv(i: Inv[Inv[String]]) = i match {
    case l: Inv[a] =>
      val x: a = l.head
      x.head: String // okay
  }

  def cov(c: Cov[Cov[String]]) = c match {
    case l: Cov[a] =>
      val x: a = l.head
      x.head: String // was: found A, required String
  }

  def cov1(c: Cov[Cov[String]]) = c match {
    case l: Cov[a] => l.head.head
  }
  cov1(null): String // was: found A, required String

  def cov3(c: Cov[Cov[String]]): String = c match {
    case l: Cov[a] => val l1: l.type = l; l1.head.head
  }
}
