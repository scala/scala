package pkg {
  class A {
    protected def f(): Unit = println("A")
  }
}

import pkg.A

trait B1 { self: A =>
  private[this] var go = true
  override def f(): Unit = if (go) {
    go = false
    println("B1-a")
    self.f()
  } else
    println("B1-b")
}

trait B2 extends A {
  override def f(): Unit = {
    println("B2")
    super.f()
  }
}

trait B3 extends A { self: A =>
  private[this] var go = true
  override def f(): Unit = if (go) {
    go = false
    println("B3-a")
    self.f()
  } else {
    println("B3-b")
    super.f()
  }
}

class C1 extends A with B1
class C2 extends A with B2
class C3 extends A with B3

// test case from pull request comment

package l1 {
  class I {
    class A {
      protected def f(): Unit = println("A")
    }
  }
  object O extends I
}

package l2 {
  class I {
    trait B4 { self: l1.O.A =>
      private[this] var go = true
      override def f(): Unit = if (go) {
        go = false
        println("B4-a")
        self.f()
      } else {
        println("B4-b")
      }
    }

    trait B5 extends l1.O.A { self: l1.O.A =>
      private[this] var go = true
      override def f(): Unit = if (go) {
        go = false
        println("B5-a")
        self.f()
      } else {
        println("B5-b")
        super.f()
      }
    }
  }
  object O extends I
}

class C4 extends l1.O.A with l2.O.B4
class C5 extends l1.O.A with l2.O.B5


object Test {
  def main(args: Array[String]): Unit = {
    new C1().f()
    new C2().f()
    new C3().f()
    new C4().f()
    new C5().f()
  }
}
