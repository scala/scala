
package mypkg {

  trait M { type C <: M }
  trait F extends M { type C = F }

  private[mypkg] abstract class Private[E <: M] {
    type Sub <: Private[E#C]
    final val sub: Sub = null.asInstanceOf[Sub]

    def foo: E
  }

  trait Public[E <: M] extends Private[E] {
    type Sub = Public[E#C]
  }
}

package p {

  import mypkg._

  object Test {
    val seq = null.asInstanceOf[Public[F]]
    seq.sub.foo
  }
}

/* private[mypkg] is permitted but unqualified private is disallowed:
 *
test/files/pos/t4559.scala:18: error: private class Private escapes its defining scope as part of type mypkg.Private[E]
  trait Public[E <: M] extends Private[E] {
                               ^
test/files/pos/t4559.scala:29: error: value sub is not a member of mypkg.Public[mypkg.F]
    seq.sub.foo
        ^
two errors found
*/
