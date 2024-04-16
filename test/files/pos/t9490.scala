//> using options -Werror -Xlint:inaccessible

package ws {
  private[ws] trait Foo
  private[ws] object Test {
    class Bar {
      def apply(f: Foo) = ???
    }
  }
}

package p {
  private[p] class D
  sealed trait T { def f(d: D): Unit }
  final class C extends T { def f(d: D) = () }
}

/* was:
t9490.scala:7: warning: method apply in class Bar references private[ws] trait Foo.
Classes which cannot access Foo may be unable to override apply.
      def apply(f: Foo) = ???
          ^
t9490.scala:14: warning: method f in trait T references private[p] class D.
Classes which cannot access D may be unable to provide a concrete implementation of f.
  sealed trait T { def f(d: D): Unit }
                       ^
 */
