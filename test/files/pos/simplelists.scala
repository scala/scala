    abstract class List[+a] {
      def head: a;
      def tail: List[a];
      def cons[b >: a](x: b): List[b] = new Cons[b, a](x, this);
    }

    object Nil extends List[All] {
      def error(msg: String): All = new java.lang.Error(msg).throw;
      def head: All = error("Nil.head");
      def tail: List[All] = error("Nil.tail");
    }

    class Cons[c, d <: c](x: c, xs: List[d]) extends List[c] {
      def head: c = x;
      def tail: List[c] = xs;
    }

