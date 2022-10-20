// scalac: -Xlint -Werror

// deprecated package should suppress deprecation warnings in the package

package p {
  @deprecated("we won't use this package any more", since="0.9-RC1")
  object `package`

  class C {
    @deprecated("this is especially useless", since="0.1")
    def c = 42
  }

  class D {
    def d = new C().c   // would be suppressed by deprecating D, should be suppressed by deprecated p
  }
}

package q {
  class K {
    def k = new p.C().c  // deprecated * 2
  }
}
