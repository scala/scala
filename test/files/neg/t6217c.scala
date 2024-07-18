//> using options -Werror
package b {
  class B
}
package object b {
  def f: B = new a.b.A().x
  def c = new a.b.C()
  def g: B = c.y
  def k: B = (new a.b.A().x: Any) match {
    case b: _root_.b.B => b
    case _ => ???
  }
}

package a {
  package b {
    class A {
      val x = new _root_.b.B
      def k: _root_.b.B = (x: Any) match {
        case b: _root_.b.B => b
        case _ => ???
      }
    }
    class C {
      import _root_.b._
      def y = new B
      def z = a._root_.X
      def v = a.b.p.Y
      def w = a._root_.p.Y
    } 
  }
  package b.p {
    object Y
  }
  package _root_ {
    object X
  }
  package _root_.p {
    object Y
  }
}
