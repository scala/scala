// scalac: -Xlint -Werror

package p {
  @deprecated("we won't use this package any more", since="0.9-RC1")
  object `package`

  package q {
    class C {
      @deprecated("this is especially useless", since="0.1")
      def c = 42
    }
  }
}
class D {
  def d = new p.q.C().c
}
