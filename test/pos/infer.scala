module test {
  class List[a] {
    def ::(x: a): List[a] = new Cons(x, this);
  }
  case class Cons[a](x: a, xs: List[a]) extends List[a];
  case class Nil[a] extends List[a];
  def nil[a]: Nil[a] = new Nil[a];
  def cons[a](x: a, xs: List[a]): List[a] = null;
  val x: List[Int] = Nil.::(1);
  val y: List[Int] = nil.::(1);
}