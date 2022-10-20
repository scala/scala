// scalac: -Xlint -Werror

// deprecated package should induce deprecation of the package members

package p {
  @deprecated("we won't use this package any more", since="0.9-RC1")
  object `package`

  class C {
    def c = 42
  }
}

package q {
  import p._
  class K {
    def k = new C()    // deprecated
    def n = k.c        // let's not go nuts
  }
}
