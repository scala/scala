
object Test extends App {
  def unreachable1(xs:Seq[Char]) = xs match {
    case Seq(x, y, _*) => x::y::Nil
    case Seq(x, y, z, w) => List(z,w) // redundant!
  }
  def unreachable2(xs:Seq[Char]) = xs match {
    case Seq(x, y, _*) => x::y::Nil
    case Seq(x, y) => List(x, y)
  }

  def not_unreachable(xs:Seq[Char]) = xs match {
    case Seq(x, y, _*) => x::y::Nil
    case Seq(x) => List(x)
  }
  def not_unreachable2(xs:Seq[Char]) = xs match {
    case Seq(x, y) => x::y::Nil
    case Seq(x, y, z, _*) => List(x,y)
  }

  def contrivedExample[A, B, C](a: A, b: B, c: C): Unit = a match {
    case b => println("matched b")
    case c => println("matched c")
    case _ => println("matched neither")
  }
}
