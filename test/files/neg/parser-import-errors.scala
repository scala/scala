package p1 {
  class A1
  class B1
  class C1
}

package p2 {
  import p1.{ A1, B1 => Q1, C1 => _ }

  class X {
    new A1
    new B1
    new C1
    new Q1
  }
}

package p3 {
  import p1.{ A1 => X1, X1 => A1 }
  class X {
    new A1
    new B1
    new C1
    new X1
  }
}

package p4 {
  import p1.{ A1 => B1, B1 => A1 }
  class X {
    new A1
    new B1
    new C1
  }
}

package p5 {
  import p1.{ A1, B1 => A1 }
  class X {
    new A1
  }
}

package p6 {
  import p1.{ Z1, B1 => Z1 }
  class X {
    new Z1
  }
}
