
import p._

package p {
  class Y {
    // import is unused, X is available
    // strictly, X is ambiguous because both imported
    // defined in p, but cut them some slack
    def f = X.x
  }
}

package q {
  class Y {
    // import is used, albeit casually scoped
    // test code using this idiom was fixed by moving import to q
    def f = X.x
  }
}
