package p1 {
  abstract class T1 {
    protected def bug(p: Int = 1): Int // without 'protected' compiles fine
  }
}
package p2 { // all being in the same package compiles fine
  import p1._
  abstract class T2 extends T1 {
    class A {
      bug()
    }
  }

  abstract class T3 extends T2 {
    class A {
      bug()
    }
  }
}

