//> using options -Xsource:3

class C {
  def lazyMap[A, B](coll: Iterable[A], f: A => B) =
    new Iterable[B] {
      def iterator = coll.iterator.map(f)    // coll is ambiguous
    }
}

/* was:
t11921.scala:5: error: type mismatch;
 found   : A => B
 required: B => B
      def iterator = coll.iterator.map(f)
                                       ^
*/
