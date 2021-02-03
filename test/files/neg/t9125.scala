
package p {
  class C
}

package q {
  object p {
    class K {
      import _root_.p
      def f() = new p.C
    }
  }
}
