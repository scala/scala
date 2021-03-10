
package p {
  class C
}

package q {
  package p {
    class K {
      import _root_.{p => pp}
      def f() = new pp.C
      def g() = new _root_.p.C
    }
  }
}
