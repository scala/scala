package scala.collection.immutable

abstract class RedBlack[A] {

  def isSmaller(x: A, y: A): boolean

  private def blacken[B](t: Tree[B]): Tree[B] = t match {
    case RedTree(k, v, l, r) => BlackTree(k, v, l, r)
    case t => t
  }
  private def mkTree[B](isBlack: boolean, k: A, v: B, l: Tree[B], r: Tree[B]) =
    if (isBlack) BlackTree(k, v, l, r) else RedTree(k, v, l, r)

  abstract class Tree[+B] {
    def isEmpty: boolean
    def isBlack: boolean
    def lookup(x: A): Tree[B]
    def update[B1 >: B](k: A, v: B1): Tree[B1] = blacken(upd(k, v))
    def delete(k: A): Tree[B] = del(k)
    def elements: Iterator[Pair[A, B]]
    def upd[B1 >: B](k: A, v: B1): Tree[B1]
    def del(k: A): Tree[B]
    def smallest: NonEmpty[B]
  }
  abstract class NonEmpty[+B] extends Tree[B] {
    def isEmpty = false
    def key: A
    def value: B
    def left: Tree[B]
    def right: Tree[B]
    def lookup(k: A): Tree[B] =
      if (isSmaller(k, key)) left.lookup(k)
      else if (isSmaller(key, k)) right.lookup(k)
      else this
    def upd[B1 >: B](k: A, v: B1): Tree[B1] = {
      def balanceLeft(isBlack: boolean, z: A, zv: B, l: Tree[B1], d: Tree[B1]) = l match {
        case RedTree(y, yv, RedTree(x, xv, a, b), c) =>
          RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
        case RedTree(x, xv, a, RedTree(y, yv, b, c)) =>
          RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
        case _ =>
          mkTree(isBlack, z, zv, l, d)
      }
      def balanceRight(isBlack: boolean, x: A, xv: B, a: Tree[B1], r: Tree[B1]) = r match {
        case RedTree(z, zv, RedTree(y, yv, b, c), d) =>
          RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
        case RedTree(y, yv, b, RedTree(z, zv, c, d)) =>
          RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
        case _ =>
          mkTree(isBlack, x, xv, a, r)
      }
      if (isSmaller(k, key)) balanceLeft(isBlack, key, value, left.upd(k, v), right)
      else if (isSmaller(key, k)) balanceRight(isBlack, key, value, left, right.upd(k, v))
      else mkTree(isBlack, k, v, left, right)
    }
    def del(k: A): Tree[B] = {
      if (isSmaller(k, key)) mkTree(isBlack, key, value, left.del(k), right)
      else if (isSmaller(key, k)) mkTree(isBlack, key, value, left, right.del(k))
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val s = right.smallest
        mkTree(isBlack, s.key, s.value, left, right.del(s.key))
      }
    }
    def smallest: NonEmpty[B] = if (left.isEmpty) this else left.smallest
    def elements: Iterator[Pair[A, B]] =
      left.elements append Iterator.single(Pair(key, value)) append right.elements
  }
  case object Empty extends Tree[Nothing] {
    def isEmpty = true
    def isBlack = true
    def lookup(k: A): Tree[Nothing] = this
    def upd[B](k: A, v: B): Tree[B] = RedTree(k, v, Empty, Empty)
    def del(k: A): Tree[Nothing] = this
    def smallest: NonEmpty[Nothing] = throw new NoSuchElementException("empty map")
    def elements: Iterator[Pair[A, Nothing]] = Iterator.empty
  }
  case class RedTree[+B](override val key: A,
                         override val value: B,
                         override val left: Tree[B],
                         override val right: Tree[B]) extends NonEmpty[B] {
    def isBlack = false
  }
  case class BlackTree[+B](override val key: A,
                           override val value: B,
                           override val left: Tree[B],
                           override val right: Tree[B]) extends NonEmpty[B] {
    def isBlack = true
  }
}

