
import p._

package p {
  class Y {
    // strictly, X is ambiguous because
    // X made available by the foreign definition in p
    // cannot shadow the import.
    // But if the imported symbol is the same,
    // cut them some slack.
    // Note that since the import is not required here,
    // the import is unused with respect to this expression,
    // for purposes of linting.
    def f = X.x
  }
}

package q {
  class Y {
    // Putting the import at the top of the file is casual,
    // but it is useful here, where the import is required.
    // Test code using this idiom was fixed by moving import
    // inside the definition of q.
    def f = X.x
  }
}

/*
test.scala:9: error: reference to X is ambiguous;
it is both defined in package p and imported subsequently by
import p._
    def f = X.x
            ^
one error found
*/
