package testview;

trait Tree[+a <% Ordered[a]] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b];
  def elements: List[a]
}

object Empty extends Tree[All] {
  def insert[b >: All <% Ordered[b]](x: b): Tree[b] = new Node(x, Empty, Empty);
  def elements: List[All] = List();
}

class Node[a <% Ordered[a]](elem: a, l: Tree[a], r: Tree[a]) extends Tree[a] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b] =
    if (x == elem) this
    else if (x < elem) new Node(elem, l insert x, r)
    else new Node(elem, l, r insert x);
  def elements: List[a] =
    l.elements ::: List(elem) ::: r.elements
}

case class Str(elem: String) extends Ordered[Str] {
  def compare[b >: Str <% Ordered[b]](that: b): int = that match {
    case that1: Str => this.elem compare that1.elem
    case _ => -(that compare this)
  }
}

object Test {
//  import O.view;

  private def toCharList(s: String): List[Char] =
    if (s.length() == 0) List()
    else s.charAt(0) :: toCharList(s.substring(1));

  def main(args: Array[String]) = {
    {
      var t: Tree[String] = Empty;
      for (val s <- args) {
	t = t insert s
      }
      System.out.println(t.elements)
    }
    {
      var t: Tree[Str] = Empty;
      for (val s <- args) {
	t = t insert Str(s)
      }
      System.out.println(t.elements)
    }
    {
      var t: Tree[List[char]] = Empty;
      for (val s <- args) {
	t = t insert toCharList(s)
      }
      System.out.println(t.elements)
    }
  }
}
