module test {
  class List[+a] {
    def ::[b >: a](x: b): List[b] = new Cons(x, this);
  }
  case class Cons[a, b <: a](x: a, xs: List[b]) extends List[a];
  case object Nil extends List[All];
  def nil[n]: List[n] = Nil;
  def cons[a](x: a, xs: List[a]): List[a] = null;
  val x: List[Int] = Nil.::(1);
  val y: List[Int] = nil.::(1);
}
