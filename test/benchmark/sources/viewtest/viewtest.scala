/*                     __                                               *\
**     ________ ___   / /  ___     Scala benchmark suite                **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package benchmarks;

/* View test class from the scala test suite */

trait Ordered[a] {
  def < (x: a): boolean;
}

object O {
  def view (x: String): Ordered[String] = new Ordered[String] {
    def < (y: String) = x.compareTo(y) < 0;
  }
}

object Empty extends Tree[All];
case class Node[+c <% Ordered[c]](elem: c, l: Tree[c], r: Tree[c]) extends Tree[c];

trait Tree[+a <% Ordered[a]] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b] = this match {
    case Empty => new Node(x, Empty, Empty)
    case Node(elem, l, r) =>
      if (x == elem) this
      else if (x < elem) Node(elem, l insert x, r)
      else Node(elem, l, r insert x);
  }
  def elements: List[a] = this match {
    case Empty => List()
    case Node(elem, l, r) =>
      l.elements ::: List(elem) ::: r.elements
  }
}

object viewtest1 with scala.testing.Benchmark {
  import O.view;

  def run: Unit = {
    var t: Tree[String] = Empty;
    val args = List("1", "2", "3", "4", "5", "7", "8", "9", "10");
    for (val s <- args) {
      t = t insert s
    }
  }
}
