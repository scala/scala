trait IntSet {
  def incl(x: Int): IntSet;
  def contains(x: Int): Boolean;
  def foreach(f: Int => Unit): Unit;
  def union(that: IntSet): IntSet;
}
module Empty extends IntSet {
  def contains(x: Int): Boolean = Boolean.False;
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty);
  def foreach(f: Int => Unit): Unit = ();
  def union(that: IntSet): IntSet = that;
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else True;
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this;
  def foreach(f: Int => Unit): Unit = {
    left foreach f;
    f(elem);
    right foreach f;
  }
  def union(that: IntSet): IntSet = (left union (right union that)) incl elem;
}
module test {
  def main = {
    val x = Empty incl 1 incl 2;
    val y = Empty incl 2 incl 3;
    x foreach java.lang.System.out.println;
    y foreach java.lang.System.out.println;
  }
}