package test;

object O {
  def view (x: String): Ordered[String] = new Ordered[String] {
    def compareTo [b >: String <% Ordered[b]](y: b): int = y match {
      case y1: String => x compareTo y1;
      case _ => -(y compareTo x)
    }
  }
  def view (x: char): Ordered[char] = new Ordered[char] {
    def compareTo [b >: char <% Ordered[b]](y: b): int = y match {
      case y1: char => x - y1;
      case _ => -(y compareTo x)
    }
  }
  def view[a <% Ordered[a]](x: List[a]): Ordered[List[a]] =
    new Ordered[List[a]] {
      def compareTo [b >: List[a] <% Ordered[b]](y: b): int = y match {
	case y1: List[a] => compareLists(x, y1);
	case _ => -(y compareTo x)
      }
      private def compareLists(xs: List[a], ys: List[a]): int = {
	if (xs.isEmpty && ys.isEmpty) 0
	else if (xs.isEmpty) -1
	else if (ys.isEmpty) 1
	else {
	  val s = xs.head compareTo ys.head;
	  if (s != 0) s
	  else compareLists(xs.tail, ys.tail)
	}
      }
    }
}

trait Tree[+a <% Ordered[a]] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b];
  def elements: List[a]
}

object Empty extends Tree[All] {
  def insert[b >: All <% Ordered[b]](x: b): Tree[b] = new Node(x, Empty, Empty);
  def elements: List[All] = List();
}

class Node[+a <% Ordered[a]](elem: a, l: Tree[a], r: Tree[a]) extends Tree[a] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b] =
    if (x == elem) this
    else if (x < elem) new Node(elem, l insert x, r)
    else new Node(elem, l, r insert x);
  def elements: List[a] =
    l.elements ::: List(elem) ::: r.elements
}

object Test {
  import O.view;

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
      var t: Tree[List[char]] = Empty;
      for (val s <- args) {
	t = t insert toCharList(s)
      }
      System.out.println(t.elements)
    }
  }
}
