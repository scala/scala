//> using options -Xsource:2.13
import q.X

package p {
  object Test {
    def f(x: q.X.type) = ???
    def test = f(X)
  }
}

/* Should be ambiguous, but was:
test/files/neg/t2458/test.scala:7: error: type mismatch;
 found   : X (in p)
 required: X (in q)
    def test = f(X)
                 ^
one error found
*/
