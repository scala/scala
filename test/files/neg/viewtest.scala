package test

/** A trait for totally ordered data.
 */
trait Ordered[+a] {

  /** Result of comparing `this' with operand `that'.
   *  returns `x' where
   *  x < 0    iff    this < that
   *  x == 0   iff    this == that
   *  x > 0    iff    this > that
   */
  def compareTo [b >: a <% Ordered[b]](that: b): Int

  def <  [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) <  0

  def >  [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) >  0

  def <= [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) <= 0

  def >= [b >: a <% Ordered[b]](that: b): Boolean = (this compareTo that) >= 0
}


object O {

  implicit def view1(x: String): Ordered[String] = new Ordered[String] {
    def compareTo [b >: String <% Ordered[b]](y: b): Int = y match {
      case y1: String => x compareTo y1
      case _ => -(y compareTo x)
    }
  }
  implicit def view2(x: Char): Ordered[Char] = new Ordered[Char] {
    def compareTo [b >: Char <% Ordered[b]](y: b): Int = y match {
      case y1: Char => x - y1
      case _ => -(y compareTo x)
    }
  }

  implicit def view3[a <% Ordered[a]](x: List[a]): Ordered[List[a]] = 
    new Ordered[List[a]] {
      def compareTo [b >: List[a] <% Ordered[b]](y: b): Int = y match {
        case y1: List[a] => compareLists(x, y1)
        case _ => -(y compareTo x)
      }
      private def compareLists(xs: List[a], ys: List[a]): Int = {
        if (xs.isEmpty && ys.isEmpty) 0
        else if (xs.isEmpty) -1
        else if (ys.isEmpty) 1
        else {
          val s = xs.head compareTo ys.head
          if (s != 0) s
          else compareLists(xs.tail, ys.tail)
        }
      }
    }
  implicit def view4[a](x: a): a = x
}

abstract class Tree[+a <% Ordered[a]] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b]
  def elements: List[a]
}

object Empty extends Tree[Nothing] {
  def insert[b >: Nothing <% Ordered[b]](x: b): Tree[b] = new Node(x, Empty, Empty)
  def elements: List[Nothing] = List()
}

class Node[a <% Ordered[a]](elem: a, l: Tree[a], r: Tree[a]) extends Tree[a] {
  def insert[b >: a <% Ordered[b]](x: b): Tree[b] =
    if (x == elem) this
    else if (x < elem) new Node(elem, l insert x, r)
    else new Node(elem, l, r insert x)
  def elements: List[a] = 
    l.elements ::: List(elem) ::: r.elements
}

case class Str(elem: String) extends Ordered[Str] {
  def compareTo[b >: Str <% Ordered[b]](that: b): Int = that match {
    case that1: Str => this.elem compareTo that1.elem
    case _ => -(that compareTo this)
  }
}

object Test {
  import O._

  private def toCharList(s: String): List[Char] = 
    if (s.length() == 0) List()
    else s.charAt(0) :: toCharList(s.substring(1))

  def main(args: Array[String]) = {
    {
      var t: Tree[String] = Empty
      for (s <- args) {
        t = t insert s
      }
      Console.println(t.elements)
    }
    {
      var t: Tree[Str] = Empty
      for (s <- args) {
        t = t insert Str(s)
      }
      Console.println(t.elements)
    }
    {
      var t: Tree[List[Char]] = Empty
      for (s <- args) {
        t = t insert toCharList(s)
      }
      Console.println(t.elements)
    }
  }
}
