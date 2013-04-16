abstract class RedBlack[A] {
  abstract class Tree[+B]
  abstract class NonEmpty[+B] extends Tree[B] {
    def del(k: A) = {
      def balance(x: A, xv: B, tl: Tree[B], tr: Tree[B]) = (tl, tr) match {
        case (RedTree(y, yv, a, b), RedTree(z, zv, c, d)) =>
        case (RedTree(y, yv, RedTree(z, zv, a, b), c), d) =>
        case (RedTree(y, yv, a, RedTree(z, zv, b, c)), d) =>
        case (a, RedTree(y, yv, b, RedTree(z, zv, c, d))) =>
        case (a, RedTree(y, yv, RedTree(z, zv, b, c), d)) =>
        case _ =>
      }

    }
  }
  case class RedTree[+B](val key: A,
                         val value: B,
                         val left: Tree[B],
                         val right: Tree[B]) extends NonEmpty[B]
  case class BlackTree[+B](val key: A,
                           val value: B,
                           val left: Tree[B],
                           val right: Tree[B]) extends NonEmpty[B]
}
